cd gossip-information && erl -make && cd ..

erl -env ERL_LIBS .
systools:make_script("gossip", [local]).
systools:make_tar("gossip", [{erts, "/opt/homebrew/lib/erlang/"}]).

mkdir -p gossip_release
rm -rf gossip_release/*
tar -zxvf gossip.tar.gz -C gossip_release/
./start_server
