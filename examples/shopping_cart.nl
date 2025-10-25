patch($list, $iter, fn(self) {
  i = [0];

  return fn() {
    n = i[0];
    if i[0] < #self {
      i[0] = i[0] + 1;
      return self[n];
    } else {
      return null;
    }
  };
});

fn new_shopping_cart() {
  return {
    items: [],
    discount: nil,

    add_item: fn(self, new) { 
      self.items << new; 
    },

    clear: fn(self) {
      self.items = [];
    },

    raw_total: fn(self) {
      total = 0;

      for item in self.items {
        total = total + item.price;
      }

      return total;
    },

    discounted_total: fn(self) {
      if self.discount {
        return self.raw_total() * self.discount;
      } else {
        return self.raw_total();
      }
    },

    discount_amount: fn(self) {
      return self.raw_total() - self.discounted_total();
    },

    add_discount: fn(self, new) {
      self.discount = new;
    },

    render: fn(self) {
      result = "=== SHOPPING CART ===\n";

      for item in self.items {
        result << "item: {item.name}, {str(item.price)}\n";
      }

      result << "=====================\n";
      result << "RAW: {str(self.raw_total())}\n";
      result << "DISCOUNT : -{str(self.discount_amount())}\n";
      result << "TOTAL: {str(self.discounted_total())}\n";

      return result;
    }
  };
}

cart = new_shopping_cart();

cart.add_discount(0.8);

cart.add_item({name: "potatoes", price: 3.99});
cart.add_item({name: "yogurt", price: 5.99});
cart.add_item({name: "salmon", price: 10.99});

print(cart.render());
