

int main(void) {
    int x = ?;
    int r= 0;
    while (x < 255) {
        x = x + 2;
    }
    r = x % 2;


}



/*
__VERIFIER_assert(!(x % 2));
[6:]:
-$5{r} >= 0 ? bottom
$5{r} == 1 ? 0.
$5{r} >= 2 ? bottom

[7:]:
-$5{r} >= 0 ? bottom
$5{r} == 1 ? 0.
$5{r} >= 2 ? bottom
*/

