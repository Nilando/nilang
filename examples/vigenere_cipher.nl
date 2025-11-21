// Vigenere Cipher implementation
// This program encrypts text using the Vigenere cipher

// Create alphabet mapping
fn create_alphabet() {
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  char_to_num = {};
  num_to_char = [];

  i = 0;
  for c in chars {
    char_to_num[c] = i;
    num_to_char << c;  // Push character into list
    i = i + 1;
  }

  return {char_to_num: char_to_num, num_to_char: num_to_char};
}

// Convert string to uppercase (simplified - only handles lowercase letters)
fn to_upper(text) {
  lower = "abcdefghijklmnopqrstuvwxyz";
  upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  // Create lowercase to uppercase map
  map = {};
  i = 0;
  for c in lower {
    upper_char = upper[i];
    map[c] = upper_char;
    i = i + 1;
  }

  result = "";
  for c in text {
    if map[c] != null {
      result = `{result}{map[c]}`;
    } else {
      result = `{result}{c}`;
    }
  }

  return result;
}

// Check if character is a letter
fn is_letter(c, alphabet) {
  return alphabet.char_to_num[c] != null;
}

// Encrypt text using Vigenere cipher
fn vigenere_encrypt(text, key, alphabet) {
  text = to_upper(text);
  key = to_upper(key);

  if #key == 0 {
    return text;
  }

  result = "";
  key_index = 0;

  for c in text {
    if is_letter(c, alphabet) {
      // Get numeric values
      text_num = alphabet.char_to_num[c];
      key_char = key[key_index % #key];
      key_num = alphabet.char_to_num[key_char];

      // Apply Vigenere shift
      encrypted_num = (text_num + key_num) % 26;
      encrypted_char = alphabet.num_to_char[encrypted_num];

      result = `{result}{encrypted_char}`;
      key_index = key_index + 1;
    } else {
      // Non-letter characters pass through unchanged
      result = `{result}{c}`;
    }
  }

  return result;
}

// Decrypt text using Vigenere cipher
fn vigenere_decrypt(text, key, alphabet) {
  text = to_upper(text);
  key = to_upper(key);

  if #key == 0 {
    return text;
  }

  result = "";
  key_index = 0;

  for c in text {
    if is_letter(c, alphabet) {
      // Get numeric values
      text_num = alphabet.char_to_num[c];
      key_char = key[key_index % #key];
      key_num = alphabet.char_to_num[key_char];

      // Apply Vigenere reverse shift
      decrypted_num = (text_num - key_num + 26) % 26;
      decrypted_char = alphabet.num_to_char[decrypted_num];

      result = `{result}{decrypted_char}`;
      key_index = key_index + 1;
    } else {
      // Non-letter characters pass through unchanged
      result = `{result}{c}`;
    }
  }

  return result;
}

// Main program
alphabet = create_alphabet();

print("Vigenere Cipher");
print("===============");
print("");
print("Enter encryption key:");
key = read;

print("Enter text to encrypt:");
text = read;

print("");
print("Original text: ");
print(text);

encrypted = vigenere_encrypt(text, key, alphabet);
print("");
print("Encrypted text:");
print(encrypted);

decrypted = vigenere_decrypt(encrypted, key, alphabet);
print("");
print("Decrypted text:");
print(decrypted);
