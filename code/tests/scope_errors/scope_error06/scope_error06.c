int calculation(int x){
    while (x > 0){
        int y;
        y = y + 1;
        x = x - 1;
    }
    return y; // Should fail as y is not defined
}