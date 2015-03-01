package nl.henkerikvanderhoek.parser.json

import nl.henkerikvanderhoek.parser.Parser
import nl.henkerikvanderhoek.parser.{ Parser => P }

object JsonParser {
  val pJsonNull:Parser[JsonValue] =
    P.pSym("null").as(JsonNull())

  val pJsonBoolean:Parser[JsonValue] = ???

  val pJsonString:Parser[JsonValue] =
    for { str <- P.pToken(P.pString) } yield JsonString(str)

  val pJsonNumber:Parser[JsonValue] =
    for { n <- P.pToken(P.pDigits) } yield JsonNumber(n)

  val pJsonArray:Parser[JsonValue] = ???

  val pJsonObject:Parser[JsonValue] = {
    val pPair = for {
      k <- P.pString
      _ <- P.pColon
      v <- pJsonValue
    } yield (k, v)

    for { xs <- P.pBraces(P.pSepBy(pPair, P.pComma)) } yield JsonObject(xs.toMap)
  }

  val pJsonValue:Parser[JsonValue] =
    pJsonObject.choice(pJsonNumber)
               .choice(pJsonArray)
               .choice(pJsonBoolean)
               .choice(pJsonNull)
               .choice(pJsonString)
}