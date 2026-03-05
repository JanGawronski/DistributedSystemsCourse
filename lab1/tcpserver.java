import java.net.*;
import java.io.*;

public class TCPserver {
    public static void main(String[] args) {
	try (ServerSocket serverSocket = new ServerSocket(12345);) {
	    while(true) {
		Socket clientSocket = serverSocket.accept();
		PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
		BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
		String msg = in.readLine();
		System.out.println("received msg: " + msg);
		out.println("Pong");
		clientSocket.close();
	    }
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }
}
