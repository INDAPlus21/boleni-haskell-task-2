
#### 1. Varför kan man skriva utryck så som detta i Haskell?
```haskell
let evenNumbers = [ 2 * n | n <- [1..]]
print (take 1000000 evenNumbers)
```
Haskell har s.k list comprehension, vilket är som matematikens mängdbyggare, därför kan man bygga 
mängder/listor med villkor som visas i exemplet `[2 * n | n <- [1..]]`. Man tillåts även bygga oändliga mängder
/listor eftersom Haskell är ett 'lazy' programmeringsspråk vilket innebär att haskell inte räknar ut vad 
uttrycket blir förrän man explicit ber om det. Hade haskell istället räknat ut uttrycket direkt hade uträkningen aldrig
tagit slut eftersom den är oändlig. 
När vi däremot väl ber om x antal tal `take x evenNumbers` så räknar haskell då alltså ut x  antal nummer och
returnerar dessa från den eviga mängden/listan.

#### 2. Vad innebär rekursion?
Rekursion innebär att en funktion definieras med hjälp av sig själv. Det blir alltså en evig
loop om inte ett basfall finns i 'botten' av det hela. Ex Fib talföljd: `f(n) = f(n-1) + f(n-2)`, om inte basfallen
`f(0) = 0` och `f(1) = 1` bestäms så hade talföljden aldrig kunnat definieras.

#### 3. Varför fungerar följande funktion i språk så som haskell och inte i t.ex javascript
```haskell
sum :: Integral n, Num n => n -> n -> n
sum s 0 = s
sum s n = sum (s + n) (pred n)
```
pga haskell har pattern matching - dvs att de ingående argumenten till en funktion kan göra att funktionscallet
hänvisas till olika versioner av funktionen beroende på hur argumenten ser ut. Då slipper man ha massa
if-statements inuti funktionskroppen som i nedan implementation.                                                                                                    
```haskell
function sum(s, n) {
     if(n == 0){
         return s;
     } else{
         return sum(s + n, n - 1);
     }
}
```

