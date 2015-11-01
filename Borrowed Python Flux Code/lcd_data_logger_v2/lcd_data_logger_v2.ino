// A data logging LCD interface for the ML2x soil moisture probe,
// DS18B20 temperature probe, and 170mm SoftPot

//------------------------------------------------------------------------------
//#include <SD.h>
#include <String.h>
#include <SdFat.h>
#include <Wire.h>
#include "RTClib.h"
#include <OneWire.h>
#include <DallasTemperature.h>
#include <MenuSystem.h>
#include <Adafruit_MCP23017.h>
#include <Adafruit_RGBLCDShield.h>

//------------------------------------------------------------------------------
// The shield uses the I2C SCL and SDA pins. On classic Arduinos
// this is Analog 4 and 5 so you can't use those for analogRead() anymore
// However, you can connect other I2C sensors to the I2C bus and share
// the I2C bus.
Adafruit_RGBLCDShield lcd = Adafruit_RGBLCDShield();

// This is a custom LCD character for a bubble to be loaded
// into byte two--it precedes the current menu selection
byte bubble[8] = {  B00000, 
                    B01110, 
                    B11001, 
                    B11101, 
                    B11111, 
                    B10111, 
                    B01110, 
                    B00000 };

// This is a custom LCD character for a subscripted 2
// to be loaded into byte 2 of the LCD character memory
byte subTwo[8] = {  B00000,
                    B00000,
                    B00000,
                    B01100,
                    B10010,
                    B00100,
                    B01000, 
                    B11110 }; 

#define ONE_WIRE_BUS 2
#define RULER_PIN A2

// Setup a oneWire instance to communicate with any OneWire devices
// (not just Maxim/Dallas temperature ICs)
OneWire oneWire(ONE_WIRE_BUS);

// Pass our oneWire reference to Dallas Temperature. 
DallasTemperature sensors(&oneWire);

// Menu system variables
MenuSystem ms;
Menu mm("Main menu"); // Root menu
  MenuItem mm_stat("Status");
    //MenuItem stat_datetime("Date");
    //MenuItem stat_soilprobes("Soil probes");
    //MenuItem stat_rulerram("RAM");
  Menu mm_rec("Record");
    MenuItem rec_soil("Soil probes");
    MenuItem rec_off("Collar offsets");
  Menu mm_read("Read file");
    MenuItem read_soil("Soil probes");
    MenuItem read_off("Collar offsets");
  MenuItem mm_config("Configuration");

// How many readings to take for soil data
#define LOG_READS 4

// how many milliseconds to wait for button press. 1000 ms is once a second
#define POLL_INTERVAL 150 // mills between accepting button presses
unsigned long pollTime = 0;

RTC_DS1307 rtc; // define the Real Time Clock object

// The analog pins for the ML2x soil moisture probe

#define SOIL_HI_PIN A0
#define SOIL_LO_PIN A1

// The analog pin for the SoftPot ruler

#define RULER_PIN A2

// for the data logging shield, we use digital pin 10 for the SD cs line
#define CHIP_SELECT 10

// the logging file
SdFat sd;
SdFile workfile;

#define CONFIG_FILE "config.txt"
#define SOILS_FILE "soils.csv"
#define OFFSETS_FILE "offset.csv"

//------------------------------------------------------------------------------
void error(char *str)
{
  lcd.clear();
  lcd.setCursor(4,0);
  lcd.print("ERROR:");
  lcd.setCursor(0,1);
  lcd.print(str);
  
  while(1); // Termination
}

//------------------------------------------------------------------------------
void setup(void)
{ 
  // Use the 1.1V reference voltage
  analogReference(INTERNAL);
  
  // initialize the LCD
  lcd.begin(16,2);
  lcd.setBacklight(HIGH);
  lcd.setCursor(0,0);
  lcd.createChar(1, bubble);
  lcd.createChar(2, subTwo);
  
  // Initialize menu system
  mm.add_item(&mm_stat, &displayStatus);
    //mm_stat.add_item(&stat_datetime, &dispDateTime);
    //mm_stat.add_item(&stat_soilprobes, &dispSoil);
    //mm_stat.add_item(&stat_rulerram, &dispRR);
  mm.add_menu(&mm_rec);
    mm_rec.add_item(&rec_soil, &logSoil);
    mm_rec.add_item(&rec_off, &logOffsets);
  mm.add_menu(&mm_read);
    mm_read.add_item(&read_soil, &readSoil);
    mm_read.add_item(&read_off, &readOffsets);
  mm.add_item(&mm_config, &configure);
  ms.set_root_menu(&mm);
  
  //Initialize SD card
  lcd.print("Initializing");
  lcd.setCursor(0,1);
  lcd.print("SD card...");
  // make sure that the default chip select pin is set to
  // output, even if you don't use it:
  pinMode(10, OUTPUT);
  
  // see if the card is present and can be initialized:
  if (!sd.begin(CHIP_SELECT, SPI_FULL_SPEED)) {
    error("Card failed");
  }
  
  lcd.clear();
  lcd.print("SD card ready");
  
//  // read the configuration file
//  workfile = sd.open("config.txt", FILE_WRITE);
//  workfile.close();

  // connect to RTC
  Wire.begin();
  if (!rtc.begin()) {
    lcd.clear();
    lcd.print("RTC failed");
  }

  // create a new file
  //String filename = SOILS_FILE;
  //makeFilename(filename);
  
  if (! workfile.open(SOILS_FILE, O_RDWR | O_CREAT | O_AT_END)) 
  {
    error("File write fail");
  }

  if (workfile.fileSize() == 0) //soils.csv is empty; place header in line 1
  {
    workfile.println("stamp,date,time,millis,collar_id,temp,ml2x_v,vol_moist,freeram");
  }
  workfile.close();
  
  // create a new file
  //filename = OFFSETS_FILE;
  
  if (! workfile.open(OFFSETS_FILE, O_RDWR | O_CREAT | O_AT_END)) 
  {
    error("File write fail");
  }

  if (workfile.fileSize() == 0) //offset.csv is empty; place header in line 1
  {
    workfile.println("collar_id,side,length,stamp,date,time,millis,freeram");
  }
  workfile.close();
  
  displayMenu();
}

//------------------------------------------------------------------------------
void loop()
{
  if (pollInput())
  {
    uint8_t buttons = lcd.readButtons(); // Get the button presses
    if (buttons)
    {
      switch(buttons)
      {
        case BUTTON_UP: // Previous menu
          ms.prev();
          displayMenu();
          break;
        case BUTTON_DOWN: // Next menu
          ms.next();
          displayMenu();
          break;
        case BUTTON_LEFT: // Go up a menu level
          ms.back();
          displayMenu();
          break;
        case BUTTON_RIGHT: // Select menu/item
        case BUTTON_SELECT:
          ms.select();
          displayMenu();
          break;
      }
    }
  }
}

//------------------------------------------------------------------------------
boolean pollInput()
{
  if ((millis() - pollTime) > POLL_INTERVAL)
  {
    pollTime = millis();
    return true;
  }
  else
  {
    return false;
  }
}

//------------------------------------------------------------------------------
void displayMenu() {
  lcd.clear();
  lcd.setCursor(0,0);
  // Display the menu
  Menu const* cp_menu = ms.get_current_menu();
  
  lcd.write(byte(1));
  //lcd.print("Current menu name: ");
  lcd.print(cp_menu->get_name());
  
  lcd.setCursor(1,1);
  lcd.print(char(126)); // Right arrow character
  lcd.print(cp_menu->get_selected()->get_name());
}

//------------------------------------------------------------------------------
void displayStatus(MenuItem* p_menu_item)
{
  uint8_t buttons = lcd.readButtons();
  byte page = 1; // Three pages, start at 1
  while(buttons != BUTTON_LEFT)
  {
    switch(page) // Go through the three status pages
    {
      case 1:
        dispDateTime();
        break;
      case 2:
        dispSoil();
        break;
      case 3:
        dispRR();
        break;
    }
    if (pollInput())
    {
      buttons = lcd.readButtons();
      if((page > 1) && (buttons == BUTTON_UP))
      {
        page--;
        lcd.clear();
      }
      else if ((page < 3) && (buttons == BUTTON_DOWN))
      {
        page++;
        lcd.clear();
      }
    }
  }
  ms.prev();
}

//------------------------------------------------------------------------------
// Shows date and time in the two LCD lines
void dispDateTime ()
{
    DateTime now = rtc.now(); // Get the current time
    
    lcd.setCursor(0,0); // first line
    
    lcd.print(now.year(), DEC);
    lcd.print("-");
    //Print leading zero if single digit
    lcd.print(now.month() < 10? "0":NULL);
    lcd.print(now.month(), DEC);
    lcd.print('-');
    //Print leading zero if single digit
    lcd.print(now.day() < 10? "0":NULL);
    lcd.print(now.day(), DEC);
    lcd.print(" ");
    
    lcd.setCursor(0,1); // second line
    //Print leading zero if single digit
    lcd.print(now.hour() < 10? "0":NULL);
    lcd.print(now.hour(), DEC);
    lcd.print(':');
    //Print leading zero if single digit
    lcd.print(now.minute() < 10? "0":NULL);
    lcd.print(now.minute(), DEC);
    lcd.print(':');
    //Print leading zero if single digit
    lcd.print(now.second() < 10? "0":NULL);
    lcd.print(now.second(), DEC);
}

//------------------------------------------------------------------------------
// Shows soil probe data in two LCD lines
void dispSoil()
{
  float temp = reportTemp();
  float moisture = reportTheta(reportML2xV());
  
  lcd.setCursor(0,0); // first line
  lcd.print("Temp: ");
  lcd.print(temp);
  lcd.print(char(223)); //This is the degree symbol
  lcd.print("C    ");
  
  lcd.setCursor(0,1); //second line
  lcd.print("Vol H");
  lcd.write(byte(2));
  lcd.print("O: ");
  lcd.print(moisture);
  lcd.print("%  ");
}

// Shows digital ruler length and free RAM in two LCD lines
void dispRR()
{
  float length = reportRulerLen();
  int freeMem = freeRam();
  
  lcd.setCursor(0,0); // first line
  lcd.print("Length: ");
  lcd.print(length);
  lcd.print(" cm ");
  
  lcd.setCursor(0,1); // second line
  lcd.print("Free RAM: ");
  lcd.print(freeMem);
  lcd.print("k");
}

//------------------------------------------------------------------------------
// Initiate logging soil probe data
void logSoil(MenuItem* p_menu_item)
{
  uint8_t buttons;
  workfile.open(SOILS_FILE, O_RDWR | O_CREAT | O_AT_END);
  
  //These are the groups of collars
  String collarTags[3] = { "FP", "IP", "CONP"};
  
  String currentCollar;
  String nextCollar;
  
  // Send a sequential read through all the probed collars
  // Go through all the tags
  for(byte i = 0; i < 3; i++)
  {
    //Go through IDs 1-3 for each tag
    for(byte id = 1; id < 4; id++)
    {
      currentCollar = collarTags[i] + id;
      // Not at the last ID, go to next ID
      if (id < 3)
      {
        nextCollar = collarTags[i] + (id + 1);
      }
      // At the last ID, but not the last group. Go to next group
      else if (i < 2)
      {
        nextCollar = collarTags[i + 1] + "1";
      }
      // We're at the last collar
      else
      {
        nextCollar = "NULL";
      }
      
      lcd.clear();
      lcd.print("Logging ");
      lcd.print(currentCollar);
      for(byte reads = 0; reads < LOG_READS; reads++)
      {
        recordSoilData(currentCollar);
      }
      if(nextCollar != "NULL")
      {
        lcd.clear();
        lcd.print(currentCollar);
        lcd.print(" done. Push");
        lcd.setCursor(0,1);
        lcd.print("SEL to log ");
        lcd.print(nextCollar);
        buttons = lcd.readButtons();
        while(buttons != BUTTON_SELECT)
        {
          buttons = lcd.readButtons();
        }
      }
    }
  }
  workfile.close();
  ms.prev();
}

//------------------------------------------------------------------------------
// Log the offset space in the collars
void logOffsets(MenuItem* p_menu_item)
{
  uint8_t buttons;
  workfile.open(OFFSETS_FILE, O_RDWR | O_CREAT | O_AT_END);
  
  //These are the groups of collars
  String collarTags[3] = { "FG", "IG", "CON"};
  
  String currentCollar;
  String nextCollar;
  
  // Send a sequential read through all the probed collars
  // Go through all the tags
  for(byte i = 0; i < 3; i++)
  {
    //Go through IDs 1-5 for each tag
    for(byte id = 1; id < 6; id++)
    {
      currentCollar = collarTags[i] + id;
      // Not at the last ID, go to next ID
      if (id < 5)
      {
        nextCollar = collarTags[i] + (id + 1);
      }
      // At the last ID, but not the last group. Go to next group
      else if (i < 2)
      {
        nextCollar = collarTags[i + 1] + "1";
      }
      // We're at the last collar
      else
      {
        nextCollar = "NULL";
      }
      
      lcd.clear();
      lcd.print("Logging ");
      lcd.print(currentCollar);
      // Get lengths from two opposite sides of the collar
      for(byte reads = 1; reads < 3; reads++)
      {
        lcd.setCursor(0,1);
        lcd.print("side ");
        lcd.print(reads);
        lcd.print(", push SEL");
        // I hate using delay but it needs to be here to "debounce" the Select button
        delay(500);
        
        buttons = lcd.readButtons();
        while(buttons != BUTTON_SELECT)
        {
          buttons = lcd.readButtons();
        }
        recordOffsets(currentCollar, reads);
      }
      
      if(nextCollar != "NULL")
      {
        lcd.clear();
        lcd.print(currentCollar);
        lcd.print(" done. Press");
        lcd.setCursor(0,1);
        lcd.print("SEL for ");
        lcd.print(nextCollar);
        buttons = lcd.readButtons();
        while(buttons != BUTTON_SELECT)
        {
          buttons = lcd.readButtons();
        }
      }
    }
  }
  workfile.close();
  ms.prev();
}

//------------------------------------------------------------------------------
// Sends datetime information into the working file
void recordTime(unsigned long boardTime)
{
  DateTime now;

  // fetch the time
  now = rtc.now();
  // log time
  workfile.print(now.unixtime()); // seconds since 1/1/1970
  workfile.print(",");
  
  workfile.print(now.year(), DEC);
  workfile.print("-");
  //Print leading zero if single digit
  workfile.print(now.month() < 10? "0":NULL);
  workfile.print(now.month(), DEC);
  workfile.print("-");
  //Print leading zero if single digit
  workfile.print(now.day() < 10? "0":NULL);
  workfile.print(now.day(), DEC);
  workfile.print(",");
  
  //Print leading zero if single digit
  workfile.print(now.hour() < 10? "0":NULL);
  workfile.print(now.hour(), DEC);
  workfile.print(":");
  //Print leading zero if single digit
  workfile.print(now.minute() < 10? "0":NULL);
  workfile.print(now.minute(), DEC);
  workfile.print(":");
  //Print leading zero if single digit
  workfile.print(now.second() < 10? "0":NULL);
  workfile.print(now.second(), DEC);
  workfile.print(",");
  
  workfile.print(boardTime); // milliseconds since start
  //workfile.print(',');
}

//------------------------------------------------------------------------------
// Sends soil probe data to the working file
void recordSoilData(String collarID)
{
  recordTime(millis());
  workfile.print(",");

  workfile.print(collarID);
  workfile.print(",");
  
  float temp = reportTemp();
  while (temp == 85.00) // 85.00 is a bad read
  {
    temp = reportTemp();
  }
  workfile.print(temp);
  workfile.print(",");
  
  float voltage = reportML2xV();
  workfile.print(voltage,4);
  workfile.print(",");
  
  workfile.print(reportTheta(voltage));
  workfile.print(",");
  
  int freeMem = freeRam(); //Report free RAM in k
  workfile.print(freeMem);
  workfile.print('\n'); //Print endline
  
  workfile.sync();
}

//------------------------------------------------------------------------------
// Sends offset data to the working file
void recordOffsets(String collarID, byte side)
{
  workfile.print(collarID);
  workfile.print(",");
  
  workfile.print(side);
  workfile.print(",");
  
  workfile.print(reportRulerLen());
  workfile.print(",");
  
  recordTime(millis());
  workfile.print(",");
  
  int freeMem = freeRam(); //Report free RAM in k
  workfile.print(freeMem);
  workfile.print('\n'); // Print endline
  
  workfile.sync();
}

/*
void makeFilename(char filename[13])
{
  DateTime now = rtc.now();
  
  filename[0] = (now.year()/1000)%10 + '0'; //To get 1st digit from year()
  filename[1] = (now.year()/100)%10 + '0'; //To get 2nd digit from year()
  filename[2] = (now.year()/10)%10 + '0'; //To get 3rd digit from year()
  filename[3] = now.year()%10 + '0'; //To get 4th digit from year()
  
  filename[4] = now.month()/10 + '0'; //To get 1st digit from month()
  filename[5] = now.month()%10 + '0'; //To get 2nd digit from month()
  
  filename[6] = now.day()/10 + '0'; //To get 1st digit from day()
  filename[7] = now.day()%10 + '0'; //To get 2nd digit from day()
}
*/

//------------------------------------------------------------------------------
float reportRulerLen()
{
  analogReference(DEFAULT); // We need a 5V reference for the softpot
  int sensorValue = analogRead(RULER_PIN);
  // Convert the analog reading (which goes from 0 - 1023) to a voltage (0 - 5V):
  float voltage = sensorValue * (5.0 / 1023.0);
  // The response of sensor value to length is linear
  float length = (sensorValue * 0.0166) + 0.7395;
  analogReference(INTERNAL); // Return to the 1.1V reference
  
  length = roundClosestHalf(length);
  
  return length;
}

//------------------------------------------------------------------------------
// Round up or down to the closest 0.5 multiple value
float roundClosestHalf(float length)
{
  length = length * 2.0;
  length = float(round(length));
  length = length / 2.0;
  return length;
}

//------------------------------------------------------------------------------
float reportTemp()
{
  // Initialize temp sensor
  sensors.begin();
  sensors.requestTemperatures();
  float thisTemp = sensors.getTempCByIndex(0); // get temp in C
  
//  while(thisTemp == 85.00) // 85.00 gets reported for bad read
//  {
//    float thisTemp = sensors.getTempCByIndex(0); // get temp in C
//  }
  
  return thisTemp;
}

//------------------------------------------------------------------------------
float reportML2xV()
{
  int sensorDiff = analogRead(SOIL_HI_PIN) - analogRead(SOIL_LO_PIN);
  
  // Using the 1.1V internal reference voltage
  float voltage = sensorDiff * (1.1 / 1023.0);
  
  return voltage;
}

//------------------------------------------------------------------------------
float reportTheta(float voltage)
{
  // theta (fraction H2O vol) = ([1.07 + 6.4V - 6.4V^2 + 4.7V^3] - a_0) / a_1
  // For organic soils a_0 = 1.3 and a_1 = 7.7, confirmed by calibration
  float theta = 100 * (1.07 + (6.4 * voltage) - (6.4 * pow((voltage), 2)) +
                (4.7 * pow((voltage), 3)) - 1.3)/7.7;
  
  return theta;
}

//------------------------------------------------------------------------------
// Returns the amount of free RAM available in the system
int freeRam () 
{
  extern int __heap_start, *__brkval; 
  int v; 
  return (int) &v - (__brkval == 0 ? (int) &__heap_start : (int) __brkval); 
}

//------------------------------------------------------------------------------
void readSoil(MenuItem* p_menu_item)
{
  workfile.open(SOILS_FILE, O_READ);
  
  int minLine = 0;
  int maxLine = 0;
  
  const byte lineSize = 70; // 70 chars should be long enough
  char line[lineSize]; // Container for the line
  
  while(workfile.fgets(line, lineSize))
  {
    maxLine++;
  }
  
  // We want to restrict it to today's measurements, so:
  minLine = maxLine - (LOG_READS*9) + 1; // # of log reads per 9 collars
  
  readFile(lineSize, minLine, maxLine);
  
  workfile.close();
  ms.prev();
}

//------------------------------------------------------------------------------

void readOffsets(MenuItem * p_menu_item)
{
  workfile.open(OFFSETS_FILE, O_READ);
  
  int minLine = 0;
  int maxLine = 0;
  
  const byte lineSize = 60; // 60 chars should be long enough
  char line[lineSize]; // Container for the line
  
  while(workfile.fgets(line, lineSize))
  {
    maxLine++;
  }
  
  // We want to restrict it to today's measurements, so:
  minLine = maxLine - (2*15) + 1; // 2 Measurements for 15 collars
  
  readFile(lineSize, minLine, maxLine);
  
  workfile.close();
  ms.prev();
}

void readFile(const byte lineSize, const int minLine, const int maxLine)
{
  uint8_t buttons = lcd.readButtons();
  
  int atLine = minLine;
  byte readingFrame = 0;
  
  char line[lineSize]; // Container for the line
  
  for(byte i = 0; i < lineSize; i++) // Fill line with empty spaces
  {
    line[i] = ' ';
  }
  
  while (buttons != BUTTON_SELECT)
  { 
    workfile.rewind(); // Go back to the start of the file
    for(byte i = 0; i < atLine; i++)
    {
      workfile.fgets(line, lineSize);
    }
    
    lcd.setCursor(0,0);
    for(byte i = readingFrame; i < (readingFrame + 16); i++)
    {
      lcd.write(line[i]);
    }
    workfile.fgets(line, lineSize);
    lcd.setCursor(0,1);
    for(byte i = readingFrame; i < (readingFrame + 16); i++)
    {
      lcd.write(line[i]);
    }
    
    buttons = lcd.readButtons();
    while(buttons == 0) //Nothing is being pressed
    {
      buttons = lcd.readButtons();
    }
    switch(buttons)
    {
      case BUTTON_UP:
        if (atLine > minLine)
        {
          atLine--;
        }
        break;
      case BUTTON_DOWN:
        if(atLine < (maxLine-1))
        {
          atLine++;
        }
        break;
      case BUTTON_LEFT:
        if(readingFrame > 0)
        {
          readingFrame--;
        }
        break;
      case BUTTON_RIGHT:
        if (readingFrame < (lineSize - 16))
        {
          readingFrame++;
        }
        break;
    }
  }  
}

//------------------------------------------------------------------------------
void configure(MenuItem* p_menu_item)
{
  noFeature();
}

//------------------------------------------------------------------------------
void noFeature()
{
  lcd.clear();
  lcd.print("Not implemented!");
  delay(2000);
}
