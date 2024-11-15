int calculation(int x, int y){
    bool a;
    int z;
    a = true;

    z = a + x; // This should fail as we cannot narrow (f+y) to an int.

    return z;
}