package main

import (
	"log"
	"net/http"
)

func main() {
	fs := http.FileServer(http.Dir("."))
	http.Handle("/bar/", http.StripPrefix("/bar/", fs))

	http.HandleFunc("/", func(resp http.ResponseWriter, req *http.Request) {
		resp.Write([]byte("It worked :)\n"))
	})
	log.Println("Listening...")
	log.Fatal(http.ListenAndServe(":80", nil))
}
