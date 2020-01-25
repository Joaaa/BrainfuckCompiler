char = read()
while(char) {
    lowerCaseLetter = 0
    t = subtract(char, 97)
    i = 26
    while(i) {
        if(zero(t)) {
            lowerCaseLetter = 1;
        }
        t = subtract(t, 1)
        i = subtract(i, 1)
    }
    if(lowerCaseLetter) {
        char = subtract(char, subtract(97, 65))
    }
    print(char)
    char = read()
}