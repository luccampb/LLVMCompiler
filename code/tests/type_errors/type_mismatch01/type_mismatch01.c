int calculation(int x, int y){
    int z;
    float f;

    z = f + y; // This should fail as we cannot narrow (f+y) to an int.

    return z;
}