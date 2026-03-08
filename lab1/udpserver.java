import java.net.*;
import java.io.*;

public class UDPserver {
    public static void main(String[] args) {
	try (DatagramSocket socket = new DatagramSocket(9876);) {
	    byte[] receiveBuffer = new byte[1024];
	    while(true) {
		DatagramPacket receivePacket =
		    new DatagramPacket(receiveBuffer, receiveBuffer.length);
		socket.receive(receivePacket);
		String msg = new String(receivePacket.getData());
		System.out.println("received msg: " + msg);
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
