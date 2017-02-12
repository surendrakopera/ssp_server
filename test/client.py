import json
import socket
import ssl
import struct
import logging
import time
import sys
from kutils.ksonsocket import KSONClientConnection
from threading import Thread



def _client_default_handler(json_connection, msg):
    print msg

def main(argv):
    client_connection = KSONClientConnection(port=8080, secure=False, handler=_client_default_handler)
    client_connection.connect()
    client_connection.start()
    import time
    while True:
        client_connection.send_msg(
                            {
                                "username" : "suren",
                                "password"  : "password",
                            }
                        )
        time.sleep(1)

if __name__ == "__main__":
    main(sys.argv[1:])
