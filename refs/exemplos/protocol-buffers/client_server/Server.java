package client_server;

import client_server.Protos.AddressBook;
import client_server.Protos.Person;

import com.google.protobuf.CodedInputStream;
import com.google.protobuf.CodedOutputStream;

import java.io.*;
import java.net.*;

public class Server {
  public static void main(String[] args) {
    try{
    int port = java.lang.Integer.parseInt(args[0]);
    ServerSocket srv = new ServerSocket(port);
    while (true) {
      Socket cli=srv.accept();
      CodedInputStream cis = CodedInputStream.newInstance(cli.getInputStream());
      CodedOutputStream cos = CodedOutputStream.newInstance(cli.getOutputStream());
      (new ClientHandler(cis, cos)).start();
    }
    }catch(Exception e){
      e.printStackTrace();
    }
  }
}

class ClientHandler extends Thread {
  CodedInputStream cis;
  CodedOutputStream cos;

  ClientHandler(CodedInputStream cis, CodedOutputStream cos) {
    this.cis = cis;
    this.cos = cos;
  }

  public void run() {
    try {
    while (true) {
      int len = cis.readRawLittleEndian32();
      System.out.println("Len: " + len);
      byte[] ba = cis.readRawBytes(len);
      System.out.println("Read " + len + " bytes");
      AddressBook b = AddressBook.parseFrom(ba);
      Printer.print(b);
    }
    } catch (java.io.IOException e) {
    }
  }
}

