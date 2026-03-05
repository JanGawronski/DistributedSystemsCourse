import java.net.*;
import java.io.*;

public class Server {
    public static void main(String[] args) {
	try (DatagramSocket socket = new DatagramSocket(9876);) {
	    byte[] receiveBuffer = new byte[1024];
	    while(true) {
		DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
		socket.receive(receivePacket);
		String msg = new String(receivePacket.getData()); 
		System.out.println("received message from " + receivePacket.getSocketAddress().toString() + ": " + msg);
		byte[] sendBuffer = ("Got your message: " + msg).getBytes();
		socket.send(new DatagramPacket(sendBuffer, sendBuffer.length, receivePacket.getSocketAddress()));
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
