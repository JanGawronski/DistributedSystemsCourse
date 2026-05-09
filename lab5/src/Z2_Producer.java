import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class Z2_Producer {

    private static final String FANOUT_EXCHANGE = "exchange1_fanout";
    private static final String DIRECT_EXCHANGE = "exchange1_direct";
    private static final String TOPIC_EXCHANGE = "exchange1_topic";

    private static BuiltinExchangeType parseExchangeType(String arg) {
        switch (arg.toLowerCase()) {
            case "fanout":
                return BuiltinExchangeType.FANOUT;
            case "direct":
                return BuiltinExchangeType.DIRECT;
            case "topic":
                return BuiltinExchangeType.TOPIC;
            default:
                throw new IllegalArgumentException("Unsupported exchange type: " + arg + " (use fanout/direct/topic)");
        }
    }

    private static String exchangeNameFor(BuiltinExchangeType exchangeType) {
        switch (exchangeType) {
            case FANOUT:
                return FANOUT_EXCHANGE;
            case DIRECT:
                return DIRECT_EXCHANGE;
            case TOPIC:
                return TOPIC_EXCHANGE;
            default:
                throw new IllegalStateException("Unsupported exchange type: " + exchangeType);
        }
    }

    public static void main(String[] argv) throws Exception {

        System.out.println("Z2 PRODUCER");

        BuiltinExchangeType exchangeType = BuiltinExchangeType.FANOUT;
        if (argv.length > 0) {
            exchangeType = parseExchangeType(argv[0]);
        }

        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        String exchangeName = exchangeNameFor(exchangeType);
        channel.exchangeDeclare(exchangeName, exchangeType);

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        System.out.printf("Using exchange '%s' (%s)%n", exchangeName, exchangeType.name().toLowerCase());

        while (true) {
            String routingKey = "";
            if (exchangeType != BuiltinExchangeType.FANOUT) {
                System.out.print("Enter routing key (or 'exit'): ");
                routingKey = br.readLine();
                if (routingKey == null || "exit".equalsIgnoreCase(routingKey.trim())) {
                    break;
                }
            }

            System.out.print("Enter message (or 'exit'): ");
            String message = br.readLine();

            if (message == null || "exit".equalsIgnoreCase(message.trim())) {
                break;
            }

            channel.basicPublish(exchangeName, routingKey, null, message.getBytes(StandardCharsets.UTF_8));
            System.out.printf("Sent: key='%s', message='%s'%n", routingKey, message);
        }

        channel.close();
        connection.close();
    }
}
