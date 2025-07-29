package main

import (
	"fmt"
)

// 纯函数--只返回一个值,根据其参数计算返回值,不改变任何现有值and易于测试
func getDisCountPercentage(item []string) int {
	for _, i := range item {
		if i == "book" {
			return 5
		}
	}
	return 0
}
func main() {
	newCart := ShoppingCart{}
	newCart.addItem("apple")
	n := newCart.getDisCountPercentage()
	newCart.addItem("book")
	i := newCart.getDisCountPercentage()
	fmt.Println(n)
	fmt.Println(i)
	newCart.removeItem("book")
	z := newCart.getDisCountPercentage()
	fmt.Println(z)
	fmt.Println(newCart.getItems())
	w := getDisCountPercentage(newCart.items)
	fmt.Println(w)
}
