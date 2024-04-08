package util

import scala.collection.immutable.Stream


object SExpParser:
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

  private def streamFromIterator[A](iterator: Iterator[A]): Stream[A] =
    if (iterator.hasNext) {
      iterator.next() #:: streamFromIterator(iterator)
    } else {
      Stream.empty
    }

  def parseSExp(input: String): SExp =
    val parser = new SExpParser(streamFromIterator(input.toIterator))
    parser.nextSExp()


  /* S-Expression lexer token types. */
  private trait SExpToken
  private case object LPAR extends SExpToken
  private case object RPAR extends SExpToken
  private case object EOS extends SExpToken
  private case class INT(value: Int) extends SExpToken
  private case class ID(value: String) extends SExpToken


  /* A one-off parser for S-Expressions. */
  private class SExpParser(private val input: Stream[Char]) {

    // Internally, this parser uses recursive descent.

    private val lex = new SExpLexer(input)

    /**
      * Parses an entire file.
      */
    def nextFile(): List[SExp] =
      lex.peek() match {

        case EOS => List.empty[SExp]

        case _ => {
          val head = nextSExp()
          val tail = nextFile()
          head :: tail
        }
      }

    /**
      * Parses the next S-Expression.
      */
    def nextSExp(): SExp =
      lex.peek() match {

        case EOS => sys.error("expected s-exp; got end of input")

        case LPAR => {
          lex.eatLPAR()
          val sexp = nextSExpList()
          lex.eatRPAR()
          sexp
        }

        case INT(value) => {
          lex.next(); SExp.Num(value)
        }
        case ID(value) => {
          lex.next(); SExp.Sym(value)
        }
      }

    /**
      * Parses a list of S-Expressions.
      */
    private def nextSExpList(): SExp.List = {
      var result: SExp.List = SExp.List(List())
      var peek = lex.peek()
      while (peek != RPAR) {
        val exp = nextSExp()
        result = SExp.List(result.list ++ List(exp))
        peek = lex.peek()
      }
      result

    }
  }


  private class SExpLexer(private var input: Stream[Char]) {

    /**
      * The next tokens available.
      */
    private var nextTokens: List[SExpToken] = List.empty

    /**
      * The tail (in reverse order) of the next tokens available.
      */
    private var nextTokensTail: List[SExpToken] = List.empty


    /**
      * Called when the lexer has seen a full token.
      */
    def emit(token: SExpToken) =
      nextTokensTail = token :: nextTokensTail

    /**
      * The current internal state of the lexer.
      */
    private var state: SExpLexerState = INWHITESPACE


    /* Lexical states. */
    private trait SExpLexerState {
      /**
        * Returns the new state after processing a character.
        */
      def process(c: Char): SExpLexerState

      /**
        * Returns the new state after processing end of file.
        */
      def processEOF(): SExpLexerState
    }

    private case object DONE extends SExpLexerState {
      def processEOF(): SExpLexerState = {
        emit(EOS)
        return DONE
      }

      def process(c: Char): SExpLexerState =
        sys.error("impossible state")
    }

    private case class INID(buf: List[Char]) extends SExpLexerState {

      def processEOF(): SExpLexerState = {
        emit(ID(buf.reverse.mkString))
        return DONE
      }

      def process(c: Char): SExpLexerState = {

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

    private case class INNUM(buf: List[Char]) extends SExpLexerState {

      def processEOF(): SExpLexerState = {
        emit(INT(buf.reverse.mkString.toInt))
        return DONE
      }

      def process(c: Char): SExpLexerState = {
        if (c.isDigit) {
          return INNUM(c :: buf)
        }

        emit(INT(buf.reverse.mkString.toInt))

        val old = input
        input = c #:: old
        return INWHITESPACE
      }
    }

    private case object INWHITESPACE extends SExpLexerState {

      def processEOF(): SExpLexerState = {
        emit(EOS)
        return DONE
      }

      def process(c: Char): SExpLexerState = {
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
    def peek(): SExpToken = {
      loadTokens()
      return nextTokens.head
    }

    /**
      * Pulls the next token from the input and returns it.
      */
    def next(): SExpToken = {
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

