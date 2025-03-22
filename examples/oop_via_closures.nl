fn create_user(name, email) {
    private = {
      name: name,
      email: email,
    };

    pub = {
      get_email: fn() {
        return private.email;
      },

      update_name: fn(new_name) {
        private.name = new_name;
      }
    };

    return pub;
}

user = create_user("Bob", "bobby@gmail.com");

print user.get_email(); // bobby@gmail.com
user.update_name("joe")
