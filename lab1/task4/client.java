import java.net.*;
import java.io.*;

public class Client {
    public static void main(String[] args) {
	try (DatagramSocket socket = new DatagramSocket();) {
	    InetAddress address = InetAddress.getByName("localhost");
	    byte[] sendBuffer = "Java Ping".getBytes();
	    socket.send(new DatagramPacket(sendBuffer, sendBuffer.length, address, 9876));
	    byte[] receiveBuffer = new byte[1024];
	    DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
	    socket.receive(receivePacket);
	    String msg = new String(receivePacket.getData()); 
	    System.out.println("received msg: " + msg);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
