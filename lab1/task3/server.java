import java.net.*;
import java.io.*;
import java.nio.*;

public class Server {
    public static void main(String[] args) {
	try (DatagramSocket socket = new DatagramSocket(9876);) {
	    byte[] receiveBuffer = new byte[1024];
	    while(true) {
		DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
		socket.receive(receivePacket);
		int number = ByteBuffer.wrap(receivePacket.getData()).order(ByteOrder.LITTLE_ENDIAN).getInt();
		System.out.println(number);
		number++;
		socket.send(new DatagramPacket(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(number).array(), 4, receivePacket.getSocketAddress()));
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
