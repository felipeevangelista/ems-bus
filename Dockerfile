FROM erlang:20.3.6

MAINTAINER evertonagilar <evertonagilar@gmail.com>

ENV HOME /var/opt/erlangms
ENV ERLANGMS_IN_DOCKER true

WORKDIR $HOME

RUN cd $HOME && \
	echo "Build ErlangMS from $(pwd)" && \
	git clone https://github.com/erlangms/ems-bus && \
	cd ems-bus && \
	git checkout v1.0.25.ldap && \
	./build.sh --profile=local

# Expose the ports we're interested in
EXPOSE 2300 2301 2389 4369

VOLUME ~/.erlangms
VOLUME ~/.odbc.ini

CMD ["/var/opt/erlangms/ems-bus/start.sh"]


