import java.net.*;
import java.io.*;

public class Server {
    public static void main(String[] args) {
	try (DatagramSocket socket = new DatagramSocket(9876);) {
	    while(true) {
		byte[] receiveBuffer = new byte[1024];
		DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
		socket.receive(receivePacket);
		String msg = new String(receivePacket.getData()); 
		System.out.println("received message from " + receivePacket.getSocketAddress().toString() + ": " + msg);
		byte[] sendBuffer = null;
		if (msg.startsWith("Python"))
		    sendBuffer = ("Python Pong").getBytes();
		else if (msg.startsWith("Java"))
		    sendBuffer = ("Java Pong").getBytes();
		else
		    sendBuffer = ("Unknown Pong").getBytes();
		socket.send(new DatagramPacket(sendBuffer, sendBuffer.length, receivePacket.getSocketAddress()));
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
