char = read()
char_zero = 48
counter = 0
while(char) {
    counter = add(counter, 1);
    char = read();
}
print(add(char_zero, counter));