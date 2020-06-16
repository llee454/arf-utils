all:

start-server:
	swipl server.pl --port=5000 --https=true --certfile="/etc/letsencrypt/live/arf.larrylee.tech/fullchain.pem" --keyfile="/etc/letsencrypt/live/arf.larrylee.tech/privkey.pem"

show-server:
	netstat -tulpn

stop-server:
	pkill -f "swipl server.pl"
