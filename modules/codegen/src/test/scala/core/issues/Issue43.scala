package core.issues

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSpec, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue43 extends FunSpec with Matchers with SwaggerSpecRunner {

  describe("Generate hierarchical classes") {

    val swagger: String = """
      | swagger: '2.0'
      | info:
      |   title: Parsing Error Sample
      |   version: 1.0.0
      | paths:
      |   /pet/{name}:
      |     get:
      |       operationId: getPet
      |       parameters:
      |         - $ref: '#/parameters/PetNamePathParam'
      |       responses:
      |         200:
      |           description: Return the details about the pet
      |           schema:
      |             $ref: '#/definitions/Pet'
      | parameters:
      |   PetNamePathParam:
      |     name: name
      |     description: Unique name of the pet
      |     in: path
      |     type: string
      |     required: true
      | definitions:
      |   Pet:
      |     type: object
      |     discriminator: petType
      |     properties:
      |       name:
      |         type: string
      |       petType:
      |         type: string
      |     required:
      |     - name
      |     - petType
      |   Cat:
      |     description: A representation of a cat
      |     allOf:
      |     - $ref: '#/definitions/Pet'
      |     - type: object
      |       properties:
      |         huntingSkill:
      |           type: string
      |           description: The measured skill for hunting
      |           default: lazy
      |           enum:
      |           - clueless
      |           - lazy
      |           - adventurous
      |           - aggressive
      |       required:
      |       - huntingSkill
      |   Dog:
      |     description: A representation of a dog
      |     allOf:
      |     - $ref: '#/definitions/Pet'
      |     - type: object
      |       properties:
      |         packSize:
      |           type: integer
      |           format: int32
      |           description: the size of the pack the dog is from
      |           default: 0
      |           minimum: 0
      |       required:
      |       - packSize""".stripMargin

    val (
      ProtocolDefinitions(
        ClassDefinition(nameCat, tpeCat, clsCat, staticDefnsCat, catParents) :: ClassDefinition(nameDog, tpeDog, _, _, _) :: ADT(
          namePet,
          tpePet,
          trtPet,
          staticDefns
        ) :: Nil,
        _,
        _,
        _
      ),
      _,
      _
    )                = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val companion    = companionForStaticDefns(staticDefns)
    val companionCat = companionForStaticDefns(staticDefnsCat)

    it("should generate right name of pets") {
      nameCat shouldBe "Cat"
      nameDog shouldBe "Dog"
      namePet shouldBe "Pet"
    }

    it("should be right type of pets") {
      tpeCat.structure shouldBe t"Cat".structure
      tpeDog.structure shouldBe t"Dog".structure
      tpePet.structure shouldBe t"Pet".structure
    }

    it("should be parent-child relationship") {
      catParents.length shouldBe 1
      catParents.headOption.map(_.clsName) shouldBe Some("Pet")
    }

    it("should generate right case class") {
      clsCat.structure shouldBe q"""case class Cat(name: String, huntingSkill: String = "lazy") extends Pet""".structure
    }

    it("should generate right companion object") {
      companionCat.toString.replaceAll("\n", "") shouldBe
        """object Cat {
          |  implicit val encodeCat = {
          |    val readOnlyKeys = Set[String]()
          |    Encoder.forProduct2("name", "huntingSkill") { (o: Cat) => (o.name, o.huntingSkill) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          |  }
          |  implicit val decodeCat = Decoder.forProduct2("name", "huntingSkill")(Cat.apply _)
          |}
          |
          |""".stripMargin.replaceAll("\n", "")
    }

    it("should generate parent as trait") {
      trtPet.structure shouldBe q"trait Pet { def name: String }".structure
    }

    it("should be right parent companion object") {
      companion.structure shouldBe q"""
      object Pet {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Pet] = Encoder.instance({
          case e: Cat =>
            e.asJsonObject.add(discriminator, "Cat".asJson).asJson
          case e: Dog =>
            e.asJsonObject.add(discriminator, "Dog".asJson).asJson
        })
        implicit val decoder: Decoder[Pet] = Decoder.instance({ c =>
          val discriminatorCursor = c.downField(discriminator)
          discriminatorCursor.as[String].flatMap({
            case "Cat" =>
              c.as[Cat]
            case "Dog" =>
              c.as[Dog]
            case tpe =>
              Left(DecodingFailure("Unknown value " ++ tpe ++ " (valid: Cat, Dog)", discriminatorCursor.history))
          })
        })
      }
      """.structure
    }

  }

  describe("Generate deep hierarchical classes") {

    val swagger: String = """
     | swagger: '2.0'
     | info:
     |   title: Parsing Error Sample
     |   version: 1.0.0
     | paths:
     |   /pet/{name}:
     |     get:
     |       operationId: getPet
     |       parameters:
     |         - $ref: '#/parameters/PetNamePathParam'
     |       responses:
     |         200:
     |           description: Return the details about the pet
     |           schema:
     |             $ref: '#/definitions/Pet'
     | parameters:
     |   PetNamePathParam:
     |     name: name
     |     description: Unique name of the pet
     |     in: path
     |     type: string
     |     required: true
     | definitions:
     |   Pet:
     |     type: object
     |     discriminator: petType
     |     properties:
     |       name:
     |         type: string
     |       petType:
     |         type: string
     |     required:
     |     - name
     |     - petType
     |   Cat:
     |     description: A representation of a cat
     |     allOf:
     |     - $ref: '#/definitions/Pet'
     |     - type: object
     |       properties:
     |         huntingSkill:
     |           type: string
     |           description: The measured skill for hunting
     |           default: lazy
     |           enum:
     |           - clueless
     |           - lazy
     |           - adventurous
     |           - aggressive
     |       required:
     |       - huntingSkill
     |   PersianCat:
     |     description: A representation of a cat
     |     allOf:
     |     - $ref: '#/definitions/Cat'
     |     - type: object
     |       properties:
     |         wool:
     |           type: integer
     |           format: int32
     |           description: The length of wool
     |           default: 10
     |   Dog:
     |     description: A representation of a dog
     |     allOf:
     |     - $ref: '#/definitions/Pet'
     |     - type: object
     |       properties:
     |         packSize:
     |           type: integer
     |           format: int32
     |           description: the size of the pack the dog is from
     |           default: 0
     |           minimum: 0
     |       required:
     |       - packSize""".stripMargin

    val (
      ProtocolDefinitions(
        ClassDefinition(namePersianCat, tpePersianCat, clsPersianCat, staticDefnsPersianCat, persianCatParents)
          :: ClassDefinition(nameDog, tpeDog, clsDog, staticDefnsDog, dogParents)
          :: ADT(namePet, tpePet, trtPet, staticDefnsPet) :: ADT(nameCat, tpeCat, trtCat, staticDefnsCat) :: Nil,
        _,
        _,
        _
      ),
      _,
      _
    )                       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val companionPersianCat = companionForStaticDefns(staticDefnsPersianCat)
    val companionDog        = companionForStaticDefns(staticDefnsDog)
    val companionPet        = companionForStaticDefns(staticDefnsPet)
    val companionCat        = companionForStaticDefns(staticDefnsCat)

    it("should generate right name of pets") {
      namePet shouldBe "Pet"
      nameDog shouldBe "Dog"
      nameCat shouldBe "Cat"
      namePersianCat shouldBe "PersianCat"
    }

    it("should be right type of pets") {
      tpeCat.structure shouldBe t"Cat".structure
      tpeDog.structure shouldBe t"Dog".structure
      tpePersianCat.structure shouldBe t"PersianCat".structure
      tpePet.structure shouldBe t"Pet".structure
    }

    it("should be parent-child relationship") {

      dogParents.length shouldBe 1
      dogParents.headOption.map(_.clsName) shouldBe Some("Pet")

      persianCatParents.length shouldBe 2
      persianCatParents.map(_.clsName) shouldBe List("Cat", "Pet")

    }

    it("should generate right case class") {
      clsDog.structure shouldBe q"""case class Dog(name: String, packSize: Int = 0) extends Pet""".structure
      clsPersianCat.structure shouldBe q"""case class PersianCat(name: String, huntingSkill: String = "lazy", wool: Option[Int] = Option(10)) extends Cat""".structure
    }

    it("should generate right companion object") {
      companionDog.toString.replaceAll("\n", "") shouldBe
        """object Dog {
          |  implicit val encodeDog = {
          |    val readOnlyKeys = Set[String]()
          |    Encoder.forProduct2("name", "packSize") { (o: Dog) => (o.name, o.packSize) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          |  }
          |  implicit val decodeDog = Decoder.forProduct2("name", "packSize")(Dog.apply _)
          |}
          |
          |""".stripMargin.replaceAll("\n", "")
      companionPersianCat.toString.replaceAll("\n", "") shouldBe
        """object PersianCat {
          |  implicit val encodePersianCat = {
          |    val readOnlyKeys = Set[String]()
          |    Encoder.forProduct3("name", "huntingSkill", "wool") { (o: PersianCat) => (o.name, o.huntingSkill, o.wool) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          |  }
          |  implicit val decodePersianCat = Decoder.forProduct3("name", "huntingSkill", "wool")(PersianCat.apply _)
          |}
          |
          |""".stripMargin.replaceAll("\n", "")
    }

    it("should generate parent as trait") {
      trtPet.structure shouldBe q"trait Pet { def name: String }".structure
      trtCat.structure shouldBe q"trait Cat extends Pet { def huntingSkill: String }".structure
    }

    it("should be right parent companion object") {
      companionPet.structure shouldBe q"""
      object Pet {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Pet] = Encoder.instance({
          case e: Dog =>
            e.asJsonObject.add(discriminator, "Dog".asJson).asJson
          case e: PersianCat =>
            e.asJsonObject.add(discriminator, "PersianCat".asJson).asJson
        })
        implicit val decoder: Decoder[Pet] = Decoder.instance { c =>
          val discriminatorCursor = c.downField(discriminator)
          discriminatorCursor.as[String].flatMap({
            case "Dog" =>
              c.as[Dog]
            case "PersianCat" =>
              c.as[PersianCat]
            case tpe =>
              Left(DecodingFailure("Unknown value " ++ tpe ++ " (valid: Dog, PersianCat)", discriminatorCursor.history))
          })
        }
      }
      """.structure
      companionCat.structure shouldBe q"""
      object Cat {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Cat] = Encoder.instance({
          case e: PersianCat =>
            e.asJsonObject.add(discriminator, "PersianCat".asJson).asJson
        })
        implicit val decoder: Decoder[Cat] = Decoder.instance { c =>
          val discriminatorCursor = c.downField(discriminator)
          discriminatorCursor.as[String].flatMap({
            case "PersianCat" =>
              c.as[PersianCat]
            case tpe =>
              Left(DecodingFailure("Unknown value " ++ tpe ++ " (valid: PersianCat)", discriminatorCursor.history))
          })
        }
      }
      """.structure
    }

  }

  describe("Generate hierarchical classes with empty properties") {
    val swagger: String = """
      |swagger: '2.0'
      |info:
      |  title: Parsing Error Sample
      |  version: 1.0.0
      |definitions:
      |  Pet:
      |    type: object
      |    discriminator: petType
      |    properties:
      |      name:
      |        type: string
      |      petType:
      |        type: string
      |    required:
      |    - name
      |    - petType
      |  Cat:
      |    description: A representation of a cat
      |    allOf:
      |    - $ref: '#/definitions/Pet'
      |    - type: object""".stripMargin

    val (
      ProtocolDefinitions(ClassDefinition(cls, _, defCls, _, _) :: ADT(_, _, _, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    it("Direct extension should be supported") {
      cls shouldBe "Cat"
      defCls.structure shouldBe q"""case class Cat(name: String) extends Pet""".structure
    }
  }

  describe("Only first discriminator should be used. Other should be ignored.") {

    val swagger: String = """
      | swagger: '2.0'
      | info:
      |   title: Parsing Error Sample
      |   version: 1.0.0
      | definitions:
      |   Pet:
      |     type: object
      |     discriminator: petType
      |     properties:
      |       petType:
      |         type: string
      |     required:
      |     - petType
      |   Cat:
      |     description: A representation of a cat
      |     discriminator: catBreed
      |     allOf:
      |     - $ref: '#/definitions/Pet'
      |     - type: object
      |       properties:
      |         catBreed:
      |           type: string
      |       required:
      |       - catBreed
      |   Dog:
      |     description: A representation of a dog
      |     allOf:
      |     - $ref: '#/definitions/Pet'
      |     - type: object
      |   PersianCat:
      |     description: A representation of a persian cat
      |     allOf:
      |     - $ref: '#/definitions/Cat'
      |     - type: object
      |
      |     """.stripMargin

    val (
      ProtocolDefinitions(
        ClassDefinition(nameDog, tpeDog, clsDog, staticDefnsDog, dogParents)
          :: ClassDefinition(namePersianCat, tpePersianCat, clsPersianCat, staticDefnsPersianCat, persianCatParents)
          :: ADT(namePet, tpePet, trtPet, staticDefnsPet) :: ADT(nameCat, tpeCat, trtCat, staticDefnsCat) :: Nil,
        _,
        _,
        _
      ),
      _,
      _
    )                       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val companionPersianCat = companionForStaticDefns(staticDefnsPersianCat)
    val companionCat        = companionForStaticDefns(staticDefnsCat)

    it("should generate right case class") {
      clsPersianCat.structure shouldBe q"""case class PersianCat(catBreed: String) extends Cat""".structure
    }

    it("should generate right companion object") {
      companionPersianCat.toString.replaceAll("\n", "") shouldBe
        """object PersianCat {
          |  implicit val encodePersianCat = {
          |    val readOnlyKeys = Set[String]()
          |    Encoder.forProduct1("catBreed") { (o: PersianCat) => o.catBreed }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          |  }
          |  implicit val decodePersianCat = Decoder.forProduct1("catBreed")(PersianCat.apply _)
          |}
          |
          |""".stripMargin.replaceAll("\n", "")
    }

    it("should generate parent as trait") {
      trtCat.structure shouldBe q"trait Cat extends Pet { def catBreed: String }".structure
    }

    it("should be right parent companion object") {
      companionCat.structure shouldBe q"""
      object Cat {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Cat] = Encoder.instance({
          case e: PersianCat =>
            e.asJsonObject.add(discriminator, "PersianCat".asJson).asJson
        })
        implicit val decoder: Decoder[Cat] = Decoder.instance { c =>
          val discriminatorCursor = c.downField(discriminator)
          discriminatorCursor.as[String].flatMap({
            case "PersianCat" =>
              c.as[PersianCat]
            case tpe =>
              Left(DecodingFailure("Unknown value " ++ tpe ++ " (valid: PersianCat)", discriminatorCursor.history))
          })
        }
      }
      """.structure
    }

  }

  describe("Support multiple inheritance.") {

    val swagger: String =
      """
        | swagger: '2.0'
        | info:
        |   title: Parsing Error Sample
        |   version: 1.0.0
        | definitions:
        |   Pet:
        |     type: object
        |     discriminator: petType
        |     properties:
        |       petType:
        |         type: string
        |     required:
        |     - petType
        |   Mammal:
        |     type: object
        |     discriminator: mammalType
        |     properties:
        |       mammalType:
        |         type: string
        |       wool:
        |         type: boolean
        |     required:
        |     - mammalType
        |     - wool
        |   Cat:
        |     description: A representation of a cat
        |     allOf:
        |     - $ref: '#/definitions/Pet'
        |     - $ref: '#/definitions/Mammal'
        |     - type: object
        |       properties:
        |         catBreed:
        |           type: string
        |       required:
        |       - catBreed
        |     """.stripMargin

    val (
      ProtocolDefinitions(
        ClassDefinition(nameCat, tpeCat, clsCat, staticDefnsCat, catParents)
          :: ADT(namePet, tpePet, trtPet, staticDefnsPet) :: ADT(nameMammal, tpeMammal, trtMammal, staticDefnsMammal) :: Nil,
        _,
        _,
        _
      ),
      _,
      _
    )                   = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val companionCat    = companionForStaticDefns(staticDefnsCat)
    val companionPet    = companionForStaticDefns(staticDefnsPet)
    val companionMammal = companionForStaticDefns(staticDefnsMammal)

    it("should generate right case class") {
      clsCat.structure shouldBe q"""case class Cat(wool: Boolean, catBreed: String) extends Pet with Mammal""".structure
    }

    it("should generate right companion object") {
      companionCat.toString.replaceAll("\n", "") shouldBe
        """object Cat {
          |  implicit val encodeCat = {
          |    val readOnlyKeys = Set[String]()
          |    Encoder.forProduct2("wool", "catBreed") { (o: Cat) => (o.wool, o.catBreed) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          |  }
          |  implicit val decodeCat = Decoder.forProduct2("wool", "catBreed")(Cat.apply _)
          |}
          |
          |""".stripMargin.replaceAll("\n", "")
    }

    it("should generate parent as trait") {
      trtMammal.structure shouldBe q"trait Mammal { def wool: Boolean }".structure
    }

    it("should be right parent companion object") {
      companionPet.structure shouldBe q"""
      object Pet {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Pet] = Encoder.instance({
          case e: Cat =>
            e.asJsonObject.add(discriminator, "Cat".asJson).asJson
        })
        implicit val decoder: Decoder[Pet] = Decoder.instance({ c => {
          val discriminatorCursor = c.downField(discriminator)
          discriminatorCursor.as[String].flatMap({
            case "Cat" =>
              c.as[Cat]
            case tpe =>
              Left(DecodingFailure("Unknown value " ++ tpe ++ " (valid: Cat)", discriminatorCursor.history))
          })
        } })
      }
      """.structure
      companionMammal.structure shouldBe q"""
      object Mammal {
        val discriminator: String = "mammalType"
        implicit val encoder: Encoder[Mammal] = Encoder.instance({
          case e: Cat =>
            e.asJsonObject.add(discriminator, "Cat".asJson).asJson
        })
        implicit val decoder: Decoder[Mammal] = Decoder.instance { c =>
          val discriminatorCursor = c.downField(discriminator)
          discriminatorCursor.as[String].flatMap({
            case "Cat" =>
              c.as[Cat]
            case tpe =>
              Left(DecodingFailure("Unknown value " ++ tpe ++ " (valid: Cat)", discriminatorCursor.history))
          })
        }
      }
      """.structure
    }

  }
}
