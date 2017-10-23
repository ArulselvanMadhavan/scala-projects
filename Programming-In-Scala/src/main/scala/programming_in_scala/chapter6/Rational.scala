class Rational(n:Int, d:Int) {
    require(d != 0)
    var numer: Int = n;
    var denom: Int = d;
    override def toString = n +"/" + d;
}