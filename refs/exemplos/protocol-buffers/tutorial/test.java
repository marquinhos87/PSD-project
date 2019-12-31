package tutorial;
import tutorial.Protos.AddressBook;
import tutorial.Protos.Person;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

class Writer {

  public static void main(String[] args)
    throws java.io.FileNotFoundException, java.io.IOException
  {
    Person p = createPerson();
    AddressBook a = AddressBook.newBuilder().addPerson(p).build();
    FileOutputStream os = new FileOutputStream(args[0]);
    a.writeTo(os);
    os.close();
  }

 static Person createPerson() {
    return
      Person.newBuilder()
      .setId(1234)
      .setName("John Doe")
      .setEmail("jdoe@example.com")
      .addPhone(
        Person.PhoneNumber.newBuilder()
          .setNumber("555-4321")
          .setType(Person.PhoneType.HOME))
      .build();
  }

}

class Reader {

  public static void main(String[] args)
    throws java.io.FileNotFoundException, java.io.IOException
  {
    FileInputStream is = new FileInputStream(args[0]);
    AddressBook a = AddressBook.parseFrom(is);
    print(a);
  }

  static void print(AddressBook a) {
    for (Person p : a.getPersonList()) {
      print(p);
    }
  }

  static void print(Person p) {
    System.out.println(
      "Name: " + p.getName() + "\n" +
      "Id: " + p.getId() + "\n" +
      "Email: " + p.getEmail());
    for (Person.PhoneNumber pn : p.getPhoneList()) {
      print(pn);
    }
  }

  static void print(Person.PhoneNumber pn) {
    System.out.println(pn.getType() + " phone: " + pn.getNumber());
  }

}

