package sr.ice.server;

import Demo.A;
import Demo.Calc;
import Demo.NoInput;
import com.zeroc.Ice.Current;
import java.util.Arrays;

public class CalcI implements Calc {
	private static final long serialVersionUID = -2448962912780867770L;
	long counter = 0;

	@Override
	public long add(int a, int b, Current __current) {
	    System.out.println("Servant " + __current.id + ": ADD: a = " + a + ", b = " + b + ", result = " + (a + b));
	    
	    if (a > 1000 || b > 1000) {
		try {
		    Thread.sleep(6000);
		} catch (InterruptedException ex) {
		    Thread.currentThread().interrupt();
		}
	    }
	    
	    if (__current.ctx.values().size() > 0) {
		System.out.println("There are some properties in the context");
	    }
	    
	    return a + b;
	}

	@Override
	public long subtract(int a, int b, Current __current) {
	    System.out.println("Servant " + __current.id + ": SUBTRACT: a = " + a + ", b = " + b + ", result = " + (a - b));
	    
	    if (a > 1000 || b > 1000) {
		try {
		    Thread.sleep(6000);
		} catch (InterruptedException ex) {
		    Thread.currentThread().interrupt();
		}
	    }
	    
	    if (__current.ctx.values().size() > 0) {
		System.out.println("There are some properties in the context");
	    }
	    
	    return a - b;
	}


	@Override
	public /*synchronized*/ void op(A a1, short b1, Current __current) {
	    System.out.println("Servant " + __current.id + ": OP" + (++counter));
	    try {
		Thread.sleep(500);
	    } catch (java.lang.InterruptedException ex) {
		Thread.currentThread().interrupt();
	    }
	}

	@Override
	public double avg(long[] a, Current __current) throws NoInput {
	    if (a.length == 0) {
		System.out.println("Servant " + __current.id + ": AVG: a = " + a + ", result = error NoInput");
		throw new NoInput();
	    } else {
		System.out.println("Servant " + __current.id + ": AVG: a = " + a + ", result = " + (Arrays.stream(a).mapToDouble(d -> d).average().orElse(0.0)));
		return Arrays.stream(a).mapToDouble(d -> d).average().orElse(0.0);
	    }
	    
	}
    
}
