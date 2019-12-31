package client_server_delimited;

import client_server_delimited.Protos.AddressBook;
import client_server_delimited.Protos.Person;

import java.io.*;
import java.net.*;

public class Server {
  public static void main(String[] args) {
    try{
    int port = java.lang.Integer.parseInt(args[0]);
    ServerSocket srv = new ServerSocket(port);
    while (true) {
      Socket cli=srv.accept();
      InputStream is = cli.getInputStream();
      OutputStream os = cli.getOutputStream();
      (new ClientHandler(is, os)).start();
    }
    }catch(Exception e){
      e.printStackTrace();
    }
  }
}

class ClientHandler extends Thread {
  InputStream is;
  OutputStream os;

  ClientHandler(InputStream is, OutputStream os) {
    this.is = is;
    this.os = os;
  }

  public void run() {
    try {
    while (true) {
      AddressBook b = AddressBook.parseDelimitedFrom(is);
      if (b == null) break;
      Printer.print(b);
    }
    } catch (java.io.IOException e) {
    }
  }
}

