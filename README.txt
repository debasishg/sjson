SJSON - JSON Serialization library for Scala objects

- built on top of dispatch-json (http://databinder.net/dispatch/Download)
- adds object serialization capabilities on top of dispatch-json

Test Cases in src/test/scala/sjson/json/JsonSpec.scala

- to compile 
  $ mvn compile

- to run tests
  $ mvn exec java

Here's the idea ..

I have a Scala object as ..

val addr = Address("Market Street", "San Francisco", "956871")

I can serialize out and in specifying the class. What I get back is another instance of the class specified ..

  // serialize in with class specification
  assertEquals(
    addr,
    serializer.in[Address](serializer.out(addr)))

I can also serialize in without specifying the class .. I get back a data structure JsValue, which comes with a set of extractors for extracting stuff from within it ..

  // serialize in without class specification
  val a = serializer.in(serializer.out(addr))

   OR

  val a = serializer.in[AnyRef](serializer.out(addr))

   OR

  val a = serializer.in[None.type](serializer.out(addr))

In all the above cases we get back a JsValue, which can be deconstructed using extractors :-

  // use extractors
  val c = 'city ? str
  val c(_city) = a
  assertEquals(_city, "San Francisco")

  val s = 'street ? str
  val s(_street) = a
  assertEquals(_street, "Market Street")

  val z = 'zip ? str
  val z(_zip) = a
  assertEquals(_zip, "956871")

LICENSE
-------

This software is licensed under the Apache 2 license as per http://www.apache.org/licenses/LICENSE-2.0.
