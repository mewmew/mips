package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"

	"github.com/mewmew/mips"
	"github.com/pkg/errors"
)

func main() {
	flag.Parse()
	for _, path := range flag.Args() {
		buf, err := ioutil.ReadFile(path)
		if err != nil {
			log.Fatalf("%+v", errors.WithStack(err))
		}
		if err := parse(buf); err != nil {
			log.Fatalf("%+v", err)
		}
	}
}

// parse decodes the MIPS assembly instructions of src.
func parse(src []byte) error {
	for i := 0; i < len(src); i += 4 {
		addr := 0x80010000 + uint32(i)
		//fmt.Printf("addr: %08X\n", addr)
		inst, err := mips.Decode(src[i:])
		if err != nil {
			return errors.WithStack(err)
		}
		fmt.Printf("ROM:%08X                 %s\n", addr, inst)
	}
	return nil
}
