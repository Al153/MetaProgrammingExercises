package lisp

object ast {

  trait Value

  case object Undefined extends Value

  case class B(b: Boolean) extends Value // Boolean
  case class I(n: Int) extends Value // Int
  case class S(sym: String) extends Value // Symbol
  case object N extends Value // Nil
  case class C(f: Value => Value) extends Value// continuation
  class P(var car: Value, var cdr: Value) extends Value // Pair
  {
    override def toString: String = this.asTail("(")

    def asTail(already: String): String =
      cdr match {
        case N => s"$already$car)"
        case that: P => that.asTail(s"$already$car, ")
        case _ => s"$already$car . $cdr)"
      }

    // NOTE: we don't override equals to respect referential equality
  }

  object P {
    def apply(a: Value, b: Value): P = new P(a, b)

    def unapply(v: Value): Option[(Value, Value)] = v match {
      case p: P => Some((p.car, p.cdr))
      case _ => None
    }
  }

  object InterpreterEnv {
    def apply(expr: Value, env: Env, cont: Cont): P = P(expr, P(env, P(cont, N)))

    def unapply(arg: Value): Option[(Value, Env, Cont)] = arg match {
      case P(expr, P(env: Env, P(cont: Cont, N))) => Some((expr, env, cont))
      case _ => None
    }
  }

  case class F(f: Value => Value) extends Value // Procedures
  case class FSubr(f: Value => Value) extends Value

  case class FExpr(f: Value => Value) extends Value

  // Env is a list of frames (each a list of key/value pairs)
  // We use object structures for easy reification/reflection.
  type Env = P
  // Similarly, continuations are values too...
  type Cont = C

  def list(e: Value): List[Value] = e match {
    case N => Nil
    case P(first, rest) => first :: list(rest)
  }

  def valueOf(es: List[Value]): Value = es match {
    case Nil => N
    case first :: rest => P(first, valueOf(rest))
  }
}

import lisp.ast.{P, _}

object eval {
  //def base_eval(exp: Value, env: Env, cont: Cont): Value = {
  def base_eval(exp: Value, env: Env, cont: Cont): Value = debug(s"eval ${pp.show(exp)}", env, cont) { cont =>
    exp match {
      case I(_) | B(_) => cont.f(exp)
      case _: S => eval_var(exp, env, cont)
      case _: P => eval_application(exp, env, cont)
    }
  }

  def eval_eval(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(body, N)) => base_eval(body, env, C({ v => base_eval(v, env, cont) }))
  }

  def eval_var(exp: Value, env: Env, cont: Cont): Value = exp match {
    case S(x) => cont.f(get(env, x))
  }

  def eval_quote(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(x, N)) => cont.f(x)
  }

  def eval_if(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(c, P(a, P(b, N)))) => base_eval(c, env, C {
      case B(false) => base_eval(b, env, cont)
      case B(true) => base_eval(a, env, cont)
    })
  }

  def eval_set_bang(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(S(x), P(rhs, N))) => base_eval(rhs, env, C { v =>
      cont.f(set(env, x, v))
    })
  }

  def eval_lambda(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(params, body)) => cont.f(F({ args =>
      eval_begin(body, extend(env, params, args), C { v => v })
    }))
  }

  def eval_fsubr(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(params, body)) =>
      cont.f(FSubr({ case args@InterpreterEnv(e, _, _) =>
        eval_begin(body, extend(env, params, args), C { v => v })
      }))
  }

  def eval_fexpr(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(params, body)) => cont.f(FExpr({ args =>
      eval_begin(body, extend(env, params, args), C { v => v })
    }))
  }


  def eval_begin(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(e, N) =>
      base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, C { _ => eval_begin(es, env, cont) })
  }

  private def getAssignments(value: ast.Value, env: Env, cont: Cont): Value =
    value match {
      case N => cont.f(P(N, N))
      case P(P(x: S, P(v, N)), rest) =>

        getAssignments(rest, env, C
          {
            case P(vars, vals) =>
              base_eval(v, env, C { r => cont.f(P(P(x, vars), P(r, vals))) })
          }
        )
    }

  def eval_let(exp: ast.Value, env: ast.Env, cont: ast.Cont): Value = exp match {
    case P(_, P(assignments, P(body, N))) =>
     getAssignments(assignments, env, C {case P(vars, values) =>
       base_eval(body, extend(env, vars, values), cont)
     })
  }

  def eval_letcc(exp: ast.Value, env: ast.Env, cont: ast.Cont): Value = exp match {
    case P(_, P(s: S, P(body, N))) =>
      base_eval(body, extend(env, P(s, N), P(cont, N)), cont)
  }


  def eval_define(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(r: S, body)) =>
      val p = P(r, Undefined)
      env.car = P(p, env.car)
      eval_begin(body, env, C { v =>
        p.cdr = v
        cont.f(r)
      })
  }

  def eval_application(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(fun, args) => base_eval(fun, env, C {
      case F(f) => evlist(args, env, C { vas => cont.f(f(vas)) })
      case FSubr(f) =>
        f(InterpreterEnv(exp, env, cont))
      case FExpr(f) => cont.f(f(args))
      case C(f) => evlist(args, env, C {case P(res, N) => f(res)})
    })
  }

  def evlist(exp: Value, env: Env, cont: Cont): Value = exp match {
    case N => cont.f(N)
    case P(first, rest) => base_eval(first, env, C { v => evlist(rest, env, C { vs => cont.f(P(v, vs)) }) })
  }

  def extend(env: Env, params: Value, args: Value): Env = {
    val frame = valueOf((list(params) zip list(args)).map { t => P(t._1, t._2) })
    P(frame, env)
  }

  def findFrame(frame: Value, x: String): Option[P] = frame match {
    case N => None
    case P(P(S(y), _), _) if x == y => Some(frame.asInstanceOf[P].car.asInstanceOf[P])
    case P(_, rest) => findFrame(rest, x)
  }

  def find(env: Env, x: String): P = env match {
    case P(first, rest) => findFrame(first, x) match {
      case Some(p) => p
      case None => rest match {
        case next: Env => find(next, x)
        case _ => sys.error(s"unbound variable $x")
      }
    }
  }

  def get(env: Env, x: String): Value = find(env, x).cdr

  def set(env: Env, x: String, v: Value): Value = {
    val p = find(env, x)
    p.cdr = v
    v
  }

  def toIntList(value: ast.Value): List[Int] =
    value match {
      case N => Nil
      case P(I(a), rest) => a :: toIntList(rest)
    }

  def make_init_env(): Env = P(valueOf(List(
    P(S("eq?"), F({ case P(a, P(b, N)) => B(a == b) })),
    P(S("<"), F({ case P(I(a), P(I(b), N)) => B(a < b) })),
    P(S("*"), F({ args => I(toIntList(args).product) })),
    P(S("+"), F({ args => println("Plus ARgs = " + toIntList(args)); I(toIntList(args).sum) })),
    P(S("-"), F({ case P(I(a), P(I(b), N)) => I(a - b) })),

    P(S("quote"), FSubr({ case InterpreterEnv(value, env, cont) => eval_quote(value, env, cont) })),
    P(S("if"), FSubr({ case InterpreterEnv(value, env, cont) => eval_if(value, env, cont) })),
    P(S("car"), F({ case P(P(a, _), N) => a })),
    P(S("cdr"), F({ case P(P(_, b), N) => b })),
    P(S("list"), F(l => l)),
    P(S("cons"), F({ case P(a, P(b, N)) => P(a, b) })),

    P(S("set!"), FSubr({ case InterpreterEnv(value, env, cont) => eval_set_bang(value, env, cont) })),
    P(S("lambda"), FSubr({ case InterpreterEnv(value, env, cont) => eval_lambda(value, env, cont) })),
    P(S("begin"), FSubr({ case InterpreterEnv(value, env, cont) => eval_begin(value, env, cont) })),
    P(S("define"), FSubr({ case InterpreterEnv(value, env, cont) => eval_define(value, env, cont) })),

    P(S("eval"), FSubr({ case InterpreterEnv(v, e, c) => eval_eval(v, e, c) })),

    P(S("fexpr"), FSubr({ case InterpreterEnv(v, e, c) => eval_fexpr(v, e, c) })),

    P(S("fsubr"), FSubr({ case InterpreterEnv(v, e, c) => eval_fsubr(v, e, c) })),

    P(S("let"), FSubr({ case InterpreterEnv(v, e, c) => eval_let(v, e, c) })),
    P(S("let/cc"), FSubr({ case InterpreterEnv(v, e, c) => eval_letcc(v, e, c) })),
  )), N)
}

import scala.util.parsing.combinator._

object parser extends JavaTokenParsers with PackratParsers {
  def exp: Parser[Value] =
    "#f" ^^ (_ => B(false)) |
      "#t" ^^ (_ => B(true)) |
      wholeNumber ^^ (s => I(s.toInt)) |
      """[^\s\(\)'"]+""".r ^^ (s => S(s)) |
      "'" ~> exp ^^ (s => P(S("quote"), P(s, N))) |
      "()" ^^ (_ => N) |
      "(" ~> exps <~ ")" ^^ (vs => vs)

  def exps: Parser[Value] =
    exp ~ exps ^^ { case v ~ vs => P(v, vs) } |
      exp ^^ (v => P(v, N))
}

import lisp.eval._
import lisp.parser._

object repl {
  var global_env: Env = make_init_env()

  def parse(s: String): Value = {
    val Success(e, _) = parseAll(exp, s)
    e
  }

  def evl(e: Value): Value = {
    base_eval(e, global_env, C { v => v })
  }

  def ev(s: String): Value = evl(parse(s))

  def clean(): Unit = {
    global_env = make_init_env()
  }
}

object pp {
  def addParen(p: (Boolean, String)): String = {
    val (need_paren, s) = p
    if (need_paren) "(" + s + ")" else s
  }

  def pp(v: Value): (Boolean, String) = v match {
    case B(b) => (false, if (b) "#t" else "#f")
    case I(n) => (false, n.toString)
    case S(s) => (false, s)
    case N => (true, "")
    case P(a, N) => (true, addParen(pp(a)))
    case P(a, d) =>
      val s1 = addParen(pp(a))
      val (need_paren2, s2) = pp(d)
      if (need_paren2) (true, s1 + " " + s2)
      else (true, s1 + " . " + s2)
    case _ => (false, v.toString)
  }

  def show(v: Value): String = addParen(pp(v))

  def display(v: Value): Unit = print(show(v))

  def newline(): Unit = println("")
}

import lisp.pp._
import lisp.repl._
import utils._

class lisp_Tests extends TestSuite {
  before {
    clean()
  }
  test("(factorial 6)") {
    ev("""(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1))))))""")
    assertResult(I(720))(ev("(factorial 6)"))
  }

  test("eq?") {
    assertResult(B(true))(ev("(eq? 1 1)"))
    assertResult(B(false))(ev("(eq? 1 2)"))
    assertResult(B(false))(ev("(eq? (list 1) (list 1))"))
  }

  test("(odd 7)") {
    ev(
      """(begin
(define even (lambda (n) (if (eq? n 0) #t (odd (- n 1)))))
(define odd (lambda (n) (if (eq? n 0) #f (even (- n 1)))))
)""")
    assertResult(B(true))(ev("(odd 7)"))
  }

  test("eval") {
    ev("(define x 1)")
    assertResult(I(1))(ev("(eval 'x)"))
    assertResult(I(2))(ev("(* (eval 'x) 2)"))
  }

  test("fexpr if") {
    ev("(define my-if (fexpr (c a b) (if (eval c) (eval a) (eval b))))")
    assertResult(I(1))(ev("(my-if #t 1 bad)"))
  }

  test("list") {
    // NOTE: we use `show` to compare pairs,
    // to by-pass referential equality.

    assertResult(ev("'()"))(ev("(list)"))
    assertResult(N)(ev("(list)"))

    assertResult(show(P(I(10), N)))(show(ev("(list 10)")))
    assertResult(show(P(I(10), N)))(show(ev("(cons '10 '())")))

    ev("(define history '())")
    assertResult(N)(ev("history"))
    assertResult(show(P(I(4), N)))(show(ev("(cons 4 history)")))
  }

  test("fexpr history") {
    ev("(define history '())")
    ev(
      """(define save! (fexpr (lhs rhs)
   ((lambda (old-val)
     (eval (list 'set! lhs rhs))
     (set! history (cons (list
        lhs
        old-val (eval lhs)) history)))
   (eval lhs))))""")
    ev("(define test 1)")
    ev("(save! test (* test 2))")
    assertResult(I(2))(ev("test"))
    assertResult("((test 1 2))")(show(ev("history")))
    ev("(save! test (* test 2))")
    assertResult(I(4))(ev("test"))
    assertResult("((test 2 4) (test 1 2))")(show(ev("history")))
  }

  test("fsubr1") {
    ev("(define my-exp (fsubr (exp env cont) exp))")
    assertResult("(my-exp x)")(show(ev("(my-exp x)")))
  }
  test("fsubr2") {

    ev("(define jump (fsubr (exp env cont) (eval (car (cdr exp)))))")

    assertResult(I(2))(ev("(- 1 (jump 2))"))
  }

  test("fsubr3") {

    ev("(define fall (fsubr (exp env cont) 1))")
    assertResult(I(1))(ev("(* 2 (fall))"))
    // NOTE: to work nicely with composing continuations,
    // we would have to adjust the calling conventions...
  }

  test("fsubr history") {
    ev("(define old-set! set!)")
    ev("(define history '())")
    ev(
      """(define save! (fexpr (lhs rhs)
   ((lambda (old-val)
     (eval (list 'old-set! lhs rhs))
     (old-set! history (cons (list
        lhs
        old-val (eval lhs)) history)))
   (eval lhs))))""")
    ev(
      """(set! set! (fsubr (exp env cont)
      (eval (list 'save! (car (cdr exp)) (car (cdr (cdr exp)))))
      (cont (car (cdr exp)))))""")
    ev("(define test 1)")
    ev("(set! test (* test 2))")
    assertResult(I(2))(ev("test"))
    assertResult("((test 1 2))")(show(ev("history")))
    ev("(set! test (* test 2))")
    assertResult(I(4))(ev("test"))
    assertResult("((test 2 4) (test 1 2))")(show(ev("history")))
  }

  test("let1") {
    assertResult(I(1))(ev("(let ((x 1)) x)"))
  }

  test("let2") {
    assertResult(I(2))(ev("((let ((f (lambda (x) x))) f) 2)"))
  }

  test("let3") {
    assertResult(I(3))(ev("(let ((x 1)(y 2)) (+ x y))"))
  }

  test("letcc1"){
    assertResult(I(5))(ev("(let/cc k (+ (k 5) (k 7) 6))"))
  }

  test("letcc2"){
    assertResult(I(2))(ev("(+ 1 (let/cc return (if #t (+ 4 (return 1)) 2)))"))
    assertResult(I(3))(ev("(+ 1 (let/cc return (if #f (+ 4 (return 1)) 2)))"))
  }
}

import lisp.ast._

object debug {
  val enable = false
  var depth: Int = 0
  val indentTab = " "

  def apply[T](msg: String, env: Env, cont: Cont)(body: Cont => T): T = trace[T](msg, env, cont)(body)

  def trace[T](msg: String, env: Env, cont: Cont)(body: Cont => T): T = {
    indentedDebug(s"==> ${pad(msg)}?")
    indentedDebug(env.format)
    depth += 1
    val newCont: Cont = C { v =>
      depth -= 1
      indentedDebug(s"<== ${pad(msg)} = ${pad(v.toString)}")
      cont.f(v)
    }
    body(newCont)
  }

  def padding: String = indentTab * depth

  def pad(s: String, padFirst: Boolean = false): String =
    s.split("\n").mkString(if (padFirst) padding else "", "\n" + padding, "")

  def indentedDebug(msg: String): Unit =
    if (enable) println(pad(msg, padFirst = true))

  implicit class EnvDeco(val env: Env) extends AnyVal {
    def format: String =
      "---------env-------\n" ++
        list(env).map(formatFrame).mkString("\n") ++
        "\n---------env-------\n"

    def formatFrame(frame: Value): String = list(frame).map {
      case P(S(name), body) => name + " -> " + body
    }.mkString("\n")
  }

}
