package tests.core.issues

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.tests._
import com.twilio.guardrail._
import org.scalatest.{ FunSpec, FunSuite, Matchers }
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
        ClassDefinition(nameCat, tpeCat, clsCat, companionCat, catParents) :: ClassDefinition(nameDog, tpeDog, _, _, _) :: ADT(
          namePet,
          tpePet,
          trtPet,
          companion
        ) :: Nil,
        _,
        _,
        _
      ),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

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
      companion.structure shouldBe q"""object Pet {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Pet] = Encoder.instance({
          case e: Cat =>
            e.asJsonObject.add(discriminator, "Cat".asJson).asJson
          case e: Dog =>
            e.asJsonObject.add(discriminator, "Dog".asJson).asJson
        })
        implicit val decoder: Decoder[Pet] = Decoder.instance(c => c.downField(discriminator).as[String].flatMap({
          case "Cat" =>
            c.as[Cat]
          case "Dog" =>
            c.as[Dog]
        }))
      }""".structure
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
        ClassDefinition(namePersianCat, tpePersianCat, clsPersianCat, companionPersianCat, persianCatParents)
          :: ClassDefinition(nameDog, tpeDog, clsDog, companionDog, dogParents)
          :: ADT(namePet, tpePet, trtPet, companionPet) :: ADT(nameCat, tpeCat, trtCat, companionCat) :: Nil,
        _,
        _,
        _
      ),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

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
      println(persianCatParents)
      clsPersianCat.structure shouldBe q"""case class PersianCat(huntingSkill: String = "lazy", name: String, wool: Option[Int] = Option(10)) extends Cat""".structure
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
          |    Encoder.forProduct3("huntingSkill", "name", "wool") { (o: PersianCat) => (o.huntingSkill, o.name, o.wool) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          |  }
          |  implicit val decodePersianCat = Decoder.forProduct3("huntingSkill", "name", "wool")(PersianCat.apply _)
          |}
          |
          |""".stripMargin.replaceAll("\n", "")
    }

    it("should generate parent as trait") {
      trtPet.structure shouldBe q"trait Pet { def name: String }".structure
      println(trtCat.toString())
      trtCat.structure shouldBe q"trait Cat extends Pet { def huntingSkill: String }".structure
    }

    it("should be right parent companion object") {
      companionPet.structure shouldBe q"""object Pet {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Pet] = Encoder.instance({
         case e: Dog =>
            e.asJsonObject.add(discriminator, "Dog".asJson).asJson
          case e: PersianCat =>
            e.asJsonObject.add(discriminator, "PersianCat".asJson).asJson
        })
        implicit val decoder: Decoder[Pet] = Decoder.instance(c => c.downField(discriminator).as[String].flatMap({
          case "Dog" =>
            c.as[Dog]
         case "PersianCat" =>
            c.as[PersianCat]
        }))
      }""".structure
      println(companionCat.toString())
      companionCat.structure shouldBe q"""object Cat {
        val discriminator: String = "petType"
        implicit val encoder: Encoder[Cat] = Encoder.instance({
          case e: PersianCat =>
            e.asJsonObject.add(discriminator, "PersianCat".asJson).asJson
        })
        implicit val decoder: Decoder[Cat] = Decoder.instance(c => c.downField(discriminator).as[String].flatMap({
          case "PersianCat" =>
            c.as[PersianCat]
        }))
      }""".structure
    }

  }

}
