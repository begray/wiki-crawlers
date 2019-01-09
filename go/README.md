### Wikipedia page crawler in Haskell

Web page crawler implemented in Haskell.

```sh
$ go build
$ time ./go-crawler-example -concurrency 1 -limit 50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
2019/01/09 12:05:22 start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
2019/01/09 12:05:22 concurrency level: 1, visited link limit: 50
2019/01/09 12:05:27 Done. 50 links visited.
./go-crawler-example -concurrency 1 -limit 50   0.73s user 0.20s system 18% cpu 5.055 total
$ time ./go-crawler-example -concurrency 8 -limit 50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"                                                               <<<
2019/01/09 12:05:45 start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
2019/01/09 12:05:45 concurrency level: 8, visited link limit: 50
2019/01/09 12:05:46 Done. 50 links visited.
./go-crawler-example -concurrency 8 -limit 50   0.67s user 0.20s system 64% cpu 1.351 total
```
