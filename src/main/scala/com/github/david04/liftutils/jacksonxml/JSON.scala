import com.github.david04.liftutils.jacksonxml.{JSON, JsonSerializable}

//  Copyright (c) 2014 David Miguel Antunes <davidmiguel {at} antunes.net>
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.

package com.fasterxml.jackson.module.scala.modifiers {

import com.fasterxml.jackson.module.scala.JacksonModule
import com.github.david04.liftutils.jacksonxml.JsonSerializable

private object JsonSerializableTypeModifier extends CollectionLikeTypeModifier {
  def BASE = classOf[JsonSerializable]
}

trait JsonSerializableTypeModifierModule extends JacksonModule {
  this += JsonSerializableTypeModifier
}

}

//  Copyright (c) 2014 David Miguel Antunes <davidmiguel {at} antunes.net>
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.

package com.github.david04.liftutils.jacksonxml {

import java.{util => ju}

import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.ser.Serializers
import com.fasterxml.jackson.datatype.joda.JodaModule
import com.fasterxml.jackson.module.scala.JacksonModule
import com.fasterxml.jackson.module.scala.deser.{ScalaStdValueInstantiatorsModule, UntypedObjectDeserializerModule}
import com.fasterxml.jackson.module.scala.introspect.ScalaClassIntrospectorModule

trait JsonSerializable {
  def json(): Option[String]
}

private class JsonSerializableSerializer extends JsonSerializer[JsonSerializable] {

  def serialize(value: JsonSerializable, jgen: JsonGenerator, provider: SerializerProvider) {
    value.json().foreach(v => jgen.writeRawValue(v))
  }
}

private object JsonSerializableSerializerResolver extends Serializers.Base {

  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription) = {
    val cls = javaType.getRawClass
    if (!classOf[JsonSerializable].isAssignableFrom(cls)) null
    else new JsonSerializableSerializer
  }

}

trait JsonSerializableSerializerModule extends JacksonModule {
  this += (_ addSerializers JsonSerializableSerializerResolver)
}

import com.fasterxml.jackson.module.scala._


object JSON extends ObjectMapper {


  registerModule(DefaultScalaModule)
  registerModule(new JsonSerializableSerializerModule {})
  registerModule(new JodaModule)
  setSerializationInclusion(JsonInclude.Include.NON_NULL)
  enableDefaultTyping(ObjectMapper.DefaultTyping.NON_FINAL)

  val L = new ObjectMapper {

    registerModule(
      new JacksonModule
          with IteratorModule
          with EnumerationModule
          with OptionModule
          with SeqModule
          with IterableModule
          with TupleModule
          with MapModule
          with SetModule
          with ScalaStdValueInstantiatorsModule
          with ScalaClassIntrospectorModule
          with UntypedObjectDeserializerModule
          with JsonSerializableSerializerModule {
        override def getModuleName = "DefaultScalaModule"
      })
    registerModule(new JodaModule)
  }

}

}


object Main extends App {
  println(JSON.writeValueAsString(List(1, 2, new JsonSerializable {
    def json(): Option[String] = Some("new Date()")
  })))
}