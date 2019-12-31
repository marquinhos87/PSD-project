package client_server_delimited;

import client_server_delimited.Protos.AddressBook;
import client_server_delimited.Protos.Person;

import java.io.*;
import java.net.*;

public class Client {

  public static void main(String[] args) {
    try{
    if(args.length<2)
      System.exit(1);
    String host = args[0];
    int port = Integer.parseInt(args[1]);
    Socket s = new Socket(host, port);
    InputStream is = s.getInputStream();
    OutputStream os = s.getOutputStream();
    Person p = createPerson();
    AddressBook a = AddressBook.newBuilder().addPerson(p).build();
    byte[] ba = a.toByteArray();
    while (true) {
      a.writeDelimitedTo(os);
      os.flush();
      Thread.sleep(3000);
    }
    //os.close();
    //s.shutdownOutput();
    }catch(Exception e){
      e.printStackTrace();
      System.exit(0);
    }
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
