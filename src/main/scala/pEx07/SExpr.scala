package pEx07

object SExpr {

  /*
   Altered to fit our needs no comments, no explicit true and false tokens.
   SExp.scala

   Author: Matt Might
   Site: http://matt.might.net/

   A small library to demonstrate how to parse S-Expressions by hand.

   This code is intended primarily to demonstrate Scala to new programmers.

   The grammar itself is also simplified:

    <file> ::= <s-exp>

    <s-exp> ::= <atom>
             |  '(' <s-exp-list> ')'

    <s-exp-list> ::= <sexp> <s-exp-list>
                  |

    <atom> ::= <symbol>
            |  <integer>
  */

  /* Exceptions. */
  private case class UnfinishedException() extends RuntimeException
  private case class InconvertibleException() extends RuntimeException
  private case class ImpossibleException() extends RuntimeException
  private case class ParseException(reason: String) extends RuntimeException

  /* S-Expression types */
  sealed abstract class SExpr extends Product with Serializable

  private def streamFromIterator[A](iterator: Iterator[A]): Stream[A] = {
    if (iterator.hasNext) {
      iterator.next() #:: streamFromIterator(iterator)
    } else {
      Stream.empty
    }
  }

  def parseSExpr(input: String): SExpr = {
    val parser = new SExprParser(streamFromIterator(input.toIterator))
    parser.nextSExpr()
  }

  /**
    * An integer as an S-Expression.
    */
  case class SNum(num: Int) extends SExpr

  /**
    * A symbol as an S-Expression.
    */
  case class SSym(symbol: String) extends SExpr


  case class SList(list: List[SExpr]) extends SExpr

  /* S-Expression lexer token types. */
  private trait SExprToken
  private case object LPAR extends SExprToken
  private case object RPAR extends SExprToken
  private case object EOS extends SExprToken
  private case class INT(value: Int) extends SExprToken
  private case class ID(value: String) extends SExprToken


  /* A one-off parser for S-Expressions. */
  private class SExprParser(private val input: Stream[Char]) {

    // Internally, this parser uses recursive descent.

    private val lex = new SExprLexer(input)

    /**
      * Parses an entire file.
      */
    def nextFile(): List[SExpr] =
      lex.peek() match {

        case EOS => List.empty[SExpr]

        case _ => {
          val head = nextSExpr()
          val tail = nextFile()
          head :: tail
        }
      }

    /**
      * Parses the next S-Expression.
      */
    def nextSExpr(): SExpr =
      lex.peek() match {

        case EOS => sys.error("expected s-exp; got end of input")

        case LPAR => {
          lex.eatLPAR()
          val sexpr = nextSExprList()
          lex.eatRPAR()
          sexpr
        }

        case INT(value) => {
          lex.next(); SNum(value)
        }
        case ID(value) => {
          lex.next(); SSym(value)
        }
      }

    /**
      * Parses a list of S-Expressions.
      */
    private def nextSExprList(): SList = {
      var result = SList(List())
      var peek = lex.peek()
      while (peek != RPAR) {
        val expr = nextSExpr()
        result = SList(result.list ++ List(expr))
        peek = lex.peek()
      }
      result

    }
  }


  private class SExprLexer(private var input: Stream[Char]) {

    /**
      * The next tokens available.
      */
    private var nextTokens: List[SExprToken] = List.empty

    /**
      * The tail (in reverse order) of the next tokens available.
      */
    private var nextTokensTail: List[SExprToken] = List.empty


    /**
      * Called when the lexer has seen a full token.
      */
    def emit(token: SExprToken) = {
      nextTokensTail = token :: nextTokensTail
    }

    /**
      * The current internal state of the lexer.
      */
    private var state: SExprLexerState = INWHITESPACE


    /* Lexical states. */
    private trait SExprLexerState {
      /**
        * Returns the new state after processing a character.
        */
      def process(c: Char): SExprLexerState

      /**
        * Returns the new state after processing end of file.
        */
      def processEOF(): SExprLexerState
    }

    private case object DONE extends SExprLexerState {
      def processEOF(): SExprLexerState = {
        emit(EOS)
        return DONE
      }

      def process(c: Char): SExprLexerState = {
        sys.error("impossible state")
      }
    }

    private case class INID(buf: List[Char]) extends SExprLexerState {

      def processEOF(): SExprLexerState = {
        emit(ID(buf.reverse.mkString))
        return DONE
      }

      def process(c: Char): SExprLexerState = {

        if (c.isWhitespace) {
          emit(ID(buf.reverse.mkString))
          return INWHITESPACE
        }

        c match {
          case '(' => {
            emit(ID(buf.reverse.mkString))
            emit(LPAR)
            return INWHITESPACE
          }

          case ')' => {
            emit(ID(buf.reverse.mkString))
            emit(RPAR)
            return INWHITESPACE
          }

          case _ => {
            return INID(c :: buf)
          }
        }

        sys.error("impossible state")
      }
    }

    private case class INNUM(buf: List[Char]) extends SExprLexerState {

      def processEOF(): SExprLexerState = {
        emit(INT(buf.reverse.mkString.toInt))
        return DONE
      }

      def process(c: Char): SExprLexerState = {
        if (c.isDigit) {
          return INNUM(c :: buf)
        }

        emit(INT(buf.reverse.mkString.toInt))

        val old = input
        input = c #:: old
        return INWHITESPACE
      }
    }

    private case object INWHITESPACE extends SExprLexerState {

      def processEOF(): SExprLexerState = {
        emit(EOS)
        return DONE
      }

      def process(c: Char): SExprLexerState = {
        if (c.isWhitespace)
          return INWHITESPACE

        if (c.isDigit) {
          return INNUM(List(c))
        }

        c match {
          case '(' => {
            emit(LPAR)
            return INWHITESPACE
          }
          case ')' => {
            emit(RPAR)
            return INWHITESPACE
          }

          case _ => return INID(List(c))
        }
      }
    }


    /**
      * Processes characters until the lexer emits tokens.
      */
    private def loadTokens(): Unit = {
      if (!nextTokens.isEmpty) return

        if (!nextTokensTail.isEmpty) {
          nextTokens = nextTokensTail.reverse
          nextTokensTail = List.empty
          return
        }

      if (input.isEmpty) {
        state = state.processEOF()
        // This had better load a token:
        nextTokens = nextTokensTail.reverse
        nextTokensTail = List.empty
        return
      }

      while (nextTokensTail.isEmpty && !input.isEmpty) {
        val c = input.head
        input = input.tail
        state = state.process(c)
      }

      if (input.isEmpty)
        state = state.processEOF()

      nextTokens = nextTokensTail.reverse
      nextTokensTail = List.empty
    }


    /**
      * Returns the next available token without consuming it.
      */
    def peek(): SExprToken = {
      loadTokens()
      return nextTokens.head
    }

    /**
      * Pulls the next token from the input and returns it.
      */
    def next(): SExprToken = {
      loadTokens()
      val t = nextTokens.head
      nextTokens = nextTokens.tail
      return t
    }

    /**
      * Pulls the next token from the input, failing if it's not '('.
      */
    def eatLPAR() =
      next() match {
        case LPAR => {}
        case t => sys.error("expected: '('; got: " + t)
      }

    /**
      * Pulls the next token from the input, failing if it's not ')'.
      */
    def eatRPAR() =
      next() match {
        case RPAR => {}
        case t => sys.error("expected: ')'; got: " + t)
      }
  }
}

