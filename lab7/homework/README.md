Stworzyć aplikację w środowisku Zookeeper (Java, …) która wykorzystując mechanizm obserwatorów (watches) umożliwia następujące funkcjonalności:
- Jeśli tworzony jest znode o nazwie „a” uruchamiana jest zewnętrzna aplikacja graficzna (dowolna, określona w linii poleceń),
- Jeśli jest kasowany „a” aplikacja zewnętrzna jest zatrzymywana,
- Każde dodanie potomka do „a” powoduje wyświetlenie graficznej informacji na ekranie o aktualnej ilości potomków.
Dodatkowo aplikacja powinna mieć możliwość wyświetlenia całej struktury drzewa „a”. Stworzona aplikacja powinna działać w środowisku „Replicated ZooKeeper”.
