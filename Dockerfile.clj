FROM alpine
RUN curl -O https://download.clojure.org/install/linux-install-1.10.1.478.sh
 && chmod +x linux-install-1.10.1.478.sh
 && sudo ./linux-install-1.10.1.478.sh
