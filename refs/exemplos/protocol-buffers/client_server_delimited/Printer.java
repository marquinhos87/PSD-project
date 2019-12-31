package client_server_delimited;

import client_server_delimited.Protos.AddressBook;
import client_server_delimited.Protos.Person;

class Printer {

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

