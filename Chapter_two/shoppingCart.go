package main

type ShoppingCart struct {
	items []string
}

func (shoppingCart *ShoppingCart) addItem(item string) {
	shoppingCart.items = append(shoppingCart.items, item)

}
func (shoppingCart *ShoppingCart) getDisCountPercentage() int {
	for _, i := range shoppingCart.items {
		if i == "book" {
			return 5
		}
	}
	return 0
}
func (shoppingCart *ShoppingCart) getItems() []string {
	itemcopy := make([]string, len(shoppingCart.items))
	copy(itemcopy, shoppingCart.items)
	return itemcopy
}
func (shoppingCart *ShoppingCart) removeItem(item string) {
	newCart := make([]string, len(shoppingCart.items))
	for _, i := range shoppingCart.items {
		if i != item {
			newCart = append(newCart, i)
		}
	}
	shoppingCart.items = newCart
} /*
func TipCalculator(name []string) int {
	var tippersentage int
	if len(name) > 5 {
		tippersentage = 20
	} else {
		tippersentage = 10
	}
	return tippersentage

}
*/
