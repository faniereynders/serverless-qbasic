#INCLUDE ONCE "nettobac.bas"
#INCLUDE ONCE "nettobac_http.bas"
#INCLUDE ONCE "fbJSON.bas"

FUNCTION newConn(BYVAL Ser AS nettobacServer PTR, BYVAL Con AS n2bConnection PTR) AS INTEGER
  ?!"Client connected!\n"
  RETURN 0
END FUNCTION


FUNCTION disConn(BYVAL Ser AS nettobacServer PTR, BYVAL Con AS n2bConnection PTR) AS INTEGER
  ?!"Client disconnected!\n"
  RETURN 0
END FUNCTION


DECLARE FUNCTION StrReplace (BYVAL StrEx AS STRING, BYVAL StrMask AS STRING, BYVAL StrRplce AS STRING) AS STRING


'-----------------------------------------------------------------------------'
FUNCTION StrReplace (BYVAL StrEx AS STRING, _
                     BYVAL StrMask AS STRING, _
                     BYVAL StrRplce AS STRING) AS STRING

    IF LEN(StrEx)=0 OR LEN(StrMask)>LEN(StrEx) THEN RETURN StrEx

    DIM Buffer AS STRING=StrEx
    DIM MaskSearch AS UINTEGER
    DIM MFound AS BYTE
    DIM lp AS UINTEGER=1

    DO
        MaskSearch=INSTR(lp,Buffer,StrMask)
        MFound=0

        IF MaskSearch THEN
            MFound=1:lp=MaskSearch+LEN(StrRplce)

            ''
            Buffer=LEFT(Buffer,MaskSearch-1)+ _
            StrRplce+ _
            RIGHT(Buffer,LEN(Buffer)-(MaskSearch+(LEN(StrMask)-1)))
            ''

        END IF

    LOOP WHILE MFound=1

    RETURN Buffer
END FUNCTION
'-----------------------------------------------------------------------------'

FUNCTION HTTP(code as INTEGER, result as string) AS STRING




Dim msg as string  = ""
  SELECT CASE AS CONST code
    CASE 200 : msg = "OK"
    CASE 400 : msg = "Bad Request"
    CASE 404 : msg = "Not Found"
    CASE 500 : msg = "Internal Server Error"
    CASE 501 : msg = "Not Implemented"
    CASE 505 : msg = "HTTP Version Not Supported"
    CASE ELSE : msg = "Unknown Error"
  END SELECT

RETURN !"HTTP/1.1 " + str(code) + " " + msg _
           & !"\r\nServer: BASIC-Server" _
           & !"\r\nAccept-Ranges: bytes" _
           & !"\r\nVary: Accept-Encoding" _
           & !"\r\nX-Content-Type-Options: nosniff" _
           & !"\r\nContent-Type: application/json" _
           & !"\r\nConnection: close" _
           & !"\r\nContent-Length: " + str(len(result)) _
           & !"\r\n\r\n" + result
END FUNCTION

FUNCTION newData(BYVAL Con AS n2bConnection PTR, BYREF Dat AS STRING) AS INTEGER

  dim httpmethod as string = StrReplace(Dat,mid(Dat, INSTR(Dat," ")),"")

  SELECT CASE httpmethod
  CASE "POST"
  
    dim body as string = Mid(Dat, INSTR(Dat,"{"))
    dim payload as jsonItem = JsonItem(body)
    dim query as jsonItem = JsonItem(payload["Data"]["req"]["Query"])
    dim greetRequest as string = "/greet"

    IF MID(Dat, 6, len(greetRequest)) = greetRequest THEN
      dim result as jsonItem = jsonItem()
      dim outputs as jsonItem = jsonItem()
      dim body as jsonItem = jsonItem()
      dim queueMessage as jsonItem = jsonItem()

      body.addItem("message", "Hello, " & urlDecode(query[0].value) & "!")
      queueMessage.addItem("message", "Hello from Queue, " + urlDecode(query[0].value) + "!")

      outputs.addItem("res", body)
      outputs.addItem("queue", queueMessage)

      result.addItem("Outputs", outputs)
      result.addItem("ReturnValue", body)
      
      Con->nPut(HTTP(200, result.toString()))
    ELSE
      Var e = "{""error"": ""not found""}"
      Con->nPut(HTTP(404, e))
    END IF

  CASE "GET"
    dim greetRequest as string = "/api/greet?name="

    IF MID(Dat, 5, len(greetRequest)) = greetRequest THEN
      VAR n = Mid(StrReplace(Dat, Mid(Dat,INSTR(Dat," HTTP")), ""),5+len(greetRequest))
  
      VAR t = "{""message"": ""hello " & urlDecode(n) & """}" 
      Con->nPut(HTTP(200, t))
   
    ELSE
    Var e = "{""error"": ""not found""}"

      ?"sending ERROR ...";
      Con->nPut(HTTP(404, e))
      ?!" done\n"
    END IF
  END SELECT : RETURN 0
END FUNCTION

FUNCTION doServer(BYVAL Port AS USHORT = 3490) AS INTEGER
  VAR server = NEW nettobacServer(Port) ' create server instance for Port
  WITH *server
    IF .Errr             THEN ?"error: " & *.Errr & " failed" : RETURN 1
    ?"server started (port = " & Port & ")"
    WHILE 0 = LEN(INKEY())
      VAR con = .nOpen() '&nettobacServer.nOpen();
      IF .Errr THEN
        SELECT CASE *.Errr
        CASE "server isset"                          ' drop this message (it means no connection request pending)
        CASE ELSE : ?"error: " & *.Errr & " failed"         ' show other
        END SELECT : .Errr = 0                     ' reset error message
      ELSE
        IF con THEN IF newConn(server, con)              THEN EXIT WHILE
      END IF

      FOR i AS INTEGER = UBOUND(.Slots) TO 0 STEP -1
        VAR dat = "", con = .Slots(i) '&n2bConnection* con;
        con->nGet(dat, 0)                  ' check for new message (single shot)
        IF .Errr THEN                                        ' got error
          SELECT CASE *.Errr                    ' no data, just an error
          CASE "retry"                               ' drop this message (it means no data pending)
          CASE "disconnected"                       ' peer disconnection
            IF disConn(server, con)                      THEN EXIT WHILE
            .nClose(con)                              ' close connection
          CASE ELSE : ?"error: " & *.Errr & " failed"       ' show other
          END SELECT : .Errr = 0                   ' reset error message
        END IF
        IF LEN(dat) ANDALSO newData(con, dat)            THEN EXIT WHILE
      NEXT : SLEEP 10
    WEND
  END WITH
  DELETE server : RETURN 0
END FUNCTION

END doServer(val(Command(1)))

