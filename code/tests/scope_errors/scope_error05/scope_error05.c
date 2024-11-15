int calculation(int x){
    if (x){
        int y;
        y = x + 1;
        return y;
    } else {
        return y; // Should fail as y is not defined in this scope
    }
}