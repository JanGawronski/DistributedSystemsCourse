import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.Envelope;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class Z2_Consumer {

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

        System.out.println("Z2 CONSUMER");

        BuiltinExchangeType exchangeType = BuiltinExchangeType.FANOUT;
        String consumerName = "consumer-" + ProcessHandle.current().pid();
        for (String arg : argv) {
            if (arg.startsWith("--type=")) {
                exchangeType = parseExchangeType(arg.substring("--type=".length()));
            } else if (arg.startsWith("--name=")) {
                consumerName = arg.substring("--name=".length());
            } else if ("fanout".equalsIgnoreCase(arg) || "direct".equalsIgnoreCase(arg) || "topic".equalsIgnoreCase(arg)) {
                exchangeType = parseExchangeType(arg);
            } else {
                throw new IllegalArgumentException("Unknown argument: " + arg);
            }
        }

        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        String exchangeName = exchangeNameFor(exchangeType);
        channel.exchangeDeclare(exchangeName, exchangeType);

        String queueName = channel.queueDeclare().getQueue();
        if (exchangeType == BuiltinExchangeType.FANOUT) {
            channel.queueBind(queueName, exchangeName, "");
            System.out.printf("[%s] created queue: %s (fanout)%n", consumerName, queueName);
        } else {
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
            System.out.print("Enter routing keys separated by spaces: ");
            String keysLine = br.readLine();
            if (keysLine == null || keysLine.trim().isEmpty()) {
                throw new IllegalArgumentException("At least one routing key is required for direct/topic");
            }

            String[] keys = keysLine.trim().split("\\s+");
            for (String key : keys) {
                channel.queueBind(queueName, exchangeName, key);
                System.out.printf("[%s] bound queue %s with key '%s'%n", consumerName, queueName, key);
            }
        }

        String finalConsumerName = consumerName;
        Consumer consumer = new DefaultConsumer(channel) {
            @Override
            public void handleDelivery(String consumerTag, Envelope envelope, AMQP.BasicProperties properties, byte[] body) throws IOException {
                String message = new String(body, StandardCharsets.UTF_8);
                System.out.printf("[%s] Received: key='%s', message='%s'%n", finalConsumerName, envelope.getRoutingKey(), message);
            }
        };

        System.out.printf("[%s] Waiting for messages on exchange '%s' (%s)...%n",
                consumerName,
                exchangeName,
                exchangeType.name().toLowerCase());
        channel.basicConsume(queueName, true, consumer);
    }
}
