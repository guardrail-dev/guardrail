package core

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import javax.validation.{ Validation, Validator }
import org.hibernate.validator.{ HibernateValidator, HibernateValidatorConfiguration }

trait TestImplicits {
  implicit val mapper: ObjectMapper = new ObjectMapper()
    .registerModule(new JavaTimeModule)
    .registerModule(DefaultScalaModule)
  implicit val validator: Validator = Validation
    .byProvider[HibernateValidatorConfiguration, HibernateValidator](classOf[HibernateValidator])
    .configure()
    .buildValidatorFactory()
    .getValidator
}
