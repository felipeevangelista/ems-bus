FROM erlang:20.0

MAINTAINER evertonagilar <evertonagilar@gmail.com>

ENV HOME /var/opt/erlangms
ENV ERLANGMS_IN_DOCKER true

WORKDIR $HOME

COPY deps $HOME/deps
COPY priv $HOME/priv
COPY src $HOME/src
COPY tools $HOME/tools
COPY include $HOME/include
COPY build.sh $HOME/build.sh
COPY start.sh $HOME/start.sh
COPY rebar.config $HOME/rebar.config
#COPY /etc/default/erlangms-build /etc/default/erlangms-build
#COPY $HOME/.erlangms/ $HOME/.erlangms/ 

RUN cd $HOME && \
	$HOME/build.sh --profile=local

# Expose the ports we're interested in
EXPOSE 2300 2301 2389 4369

VOLUME ~/.erlangms

CMD ["/var/opt/erlangms/start.sh"]


