TARGET	= Wrat
INSTALL	= /usr/local/stow/wrat/bin

HC			= ghc
HFLAGS	= -main-is $(TARGET)

.PHONY: all install clean reallyclean

all: $(TARGET).hs
	$(HC) $(HFLAGS) --make $(TARGET)

install: all
	mkdir -p $(INSTALL) && cp $(TARGET) $(INSTALL) && \
		chmod 755 $(INSTALL)/$(TARGET)

clean:
	rm -f $(TARGET).hi $(TARGET).o

reallyclean: clean
	rm -f $(TARGET)
