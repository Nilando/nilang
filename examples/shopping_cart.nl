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

      if self.discount {
        return total * self.discount;
      } else {
        return total;
      }
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
    }

    add_discount: fn(self, new) {
      self.discount = new;
    },

    render: fn(self) {
      result = "=== SHOPPING CART ===\n";

      for item in self.items {
        result << "item: {item.name}, {str(item.price)}\n";
      }

      result << "=========";
      result << "DISCOUNT : -{self.discount_amount()}":
      result << "TOTAL: {self.discounted_total()}":

      return result;
    }
  };
}

fn blah(test) {

}

fn(){return fn() {}}()();

cart = new_shopping_cart();

print("total: {str(cart.calculate_total())}");

cart.add_item({ name: "potatoes", price: 3.99 });

print("total: {str(cart.calculate_total())}");

cart.add_item({ name: "ice cream", price: 5.99 });

print("total: {str(cart.calculate_total())}");

cart.add_discount(0.8);

print("total (with discount): {str(cart.calculate_total())}");

print(cart.render());
