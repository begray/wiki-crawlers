package main

import (
	"bytes"
	"flag"
	"github.com/PuerkitoBio/goquery"
	"log"
	"net/http"
	"net/url"
	"os"
	"path"
	"runtime"
)

var concurrency = flag.Int("concurrency", runtime.GOMAXPROCS(0), "number of goroutines to use")
var maxDepth = flag.Int("depth", 3, "max depth to which to crawl")
var pageLimit = flag.Int("limit", 50, "limit of pages to crawl")
var outputBaseDir = flag.String("output", "./output", "output directory")

type DownloadUrl struct {
	url string
	depth int
}

type Result struct {
	url DownloadUrl
	links []string
}

func dispatch(startUrl string) {
	pending := make(map[string]bool)
	visited := make(map[string]bool)

	jobs := make(chan DownloadUrl, 1000000)
	results := make(chan Result, 1000000)

	for i := 0; i < *concurrency; i++ {
		go crawl(jobs, results)
	}

	pending[startUrl] = true

	whereToStart := DownloadUrl{startUrl, 1}
	jobs <- whereToStart

	for {
		if len(pending) <= 0 {
			log.Printf("Done. %d links visited.", len(visited))
			break
		}

		result := <- results

		//log.Printf("got a result: %s\n", result)

		visited[result.url.url] = true
		delete(pending, result.url.url)

		if len(visited) >= *pageLimit {
			log.Printf("Done. %d links visited.", len(visited))
			break
		}

		for _, link := range result.links {
			if _, present := visited[link]; !present {
				if _, crawling := pending[link]; !crawling {
					pending[link] = true
					jobs <- DownloadUrl{link, result.url.depth + 1}
				}
			}
		}

		//log.Printf("scheduled new batch of jobs, total jobs %d\n", len(jobs))
	}
}

func crawl(jobs chan DownloadUrl, results chan Result) {
	for {
		downloadUrl := <- jobs

		results <- crawlOne(downloadUrl)
	}
}

func crawlOne(downloadUrl DownloadUrl) Result {
	//log.Printf("crawling page %s\n", downloadUrl.url)

	// crawl page
	response, err := http.Get(downloadUrl.url)
	if err != nil {
		log.Printf("failed to crawl %s: %s\n", downloadUrl.url, err.Error())
		return Result{downloadUrl, []string{}}
	}

	pageUrl, err := url.Parse(downloadUrl.url)
	if err != nil {
		log.Printf("failed to parse URL %s: %s\n", downloadUrl.url, err.Error())
		return Result{downloadUrl, []string{}}
	}

	urlPath := pageUrl.EscapedPath()
	if pageUrl.RawQuery != "" {
		urlPath = urlPath + "_" + pageUrl.RawQuery
	}

	//log.Printf("going to save file: %s\n", urlPath)

	// save it to a file
	outputPath := path.Join(*outputBaseDir, urlPath)
	outputDir := path.Dir(outputPath)

	err = os.MkdirAll(outputDir, os.ModePerm)
	if err != nil {
		log.Printf("failed to create an output directory %s: %s", outputDir, err.Error())
		return Result{downloadUrl, []string{}}
	}

	output, err := os.Create(outputPath)
	if err != nil {
		log.Printf("failed to create file for output %s: %s\n", outputPath, err.Error())
		return Result{downloadUrl, []string{}}
	}

	// we need to dump content of a page to a file and extract links from it
	// not sure if it's possible to do this in one read, so here we first read all the body into a buffer
	// and then perform stuff with it
	tmpBuffer := bytes.Buffer{}
	tmpBuffer.ReadFrom(response.Body)

	output.Write(tmpBuffer.Bytes())

	var links []string
	if downloadUrl.depth < *maxDepth {
		// extract links and return
		links = extractLinks(pageUrl, tmpBuffer)
	}

	return Result{downloadUrl, links}
}

func extractLinks(pageUrl *url.URL, page bytes.Buffer) []string {
	doc, err := goquery.NewDocumentFromReader(bytes.NewReader(page.Bytes()))
	if err != nil {
		log.Printf("failed to extract links from a page: %s\n", err.Error())
		return []string{}
	}

	var links []string

	children := doc.Find("a")

	//log.Printf("found %d links on a page %s\n", len(children.Nodes), pageUrl.String())

	for _, child := range children.Nodes {
		for _, attr := range child.Attr {
			if attr.Key == "href" {
				link := attr.Val

				ref, err := url.Parse(link)
				if err != nil {
					log.Printf("failed to parse link %s: %s", link, err.Error())
					continue
				}

				// reset fragment, because we're crawling the whole page
				ref.Fragment = ""

				resolved := pageUrl.ResolveReference(ref)

				if resolved.Host != pageUrl.Host {
					// skip external links
					continue
				}

				links = append(links, resolved.String())
			}
		}
	}

	return links
}

func main() {
	flag.Parse()

	if flag.NArg() < 1 {
		log.Fatalf("missing required argument <start url>")
	}

	startUrl := flag.Arg(0)

	log.Printf("start crawling from %s", startUrl)
	log.Printf("concurrency level: %d, visited link limit: %d", *concurrency, *pageLimit)

	os.MkdirAll(*outputBaseDir, os.ModePerm)

	dispatch(startUrl)
}
