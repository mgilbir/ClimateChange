import processing.opengl.*;
import quicktime.*;
import processing.video.*;

float px, py;
float cx, cy;
float angle;
float radius;
float temp0radius;
int DEGREES_TO_PIXEL;
int MONTHS_IN_PAST;
PFont myFont;
String[] linesNorth;
String[] linesTropic;
String[] linesSouth;
int lineNumberNorth = 0;
int lineNumberTropic = 0;
int lineNumberSouth = 0;

int theYear, theMonth;
int endYear, endMonth;

String[] months2text = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};

MovieMaker mm;  // Declare MovieMaker object


void setup()
{
 
  mm = new MovieMaker(this, 800, 600, "circularTemperatures.mov", 30, MovieMaker.ANIMATION, MovieMaker.HIGH);
  size(800, 600);
  hint(ENABLE_OPENGL_4X_SMOOTH); 

  stroke(255);
  
  temp0radius = 70;
  DEGREES_TO_PIXEL = 7;
  MONTHS_IN_PAST = 300;
  
  // generate processing font from system font
  myFont = createFont("verdana", 12);
  textFont(myFont);
  
  cx = width/2;
  cy = height/2;
  
  linesNorth = loadStrings("north.tsv");
  linesTropic = loadStrings("tropic.tsv");
  linesSouth= loadStrings("south.tsv");
  
  
  theYear = 1701;
  theMonth = 1;
  endYear = 2009;
  endMonth = 10;
  



  frameRate(30);
}


void draw()
{
  background(50);
  
  if(theYear + (theMonth - 1)/12.0 < endYear + (endMonth -1 )/12.0)
  {
    processNorth();
    processTropic();
    processSouth();
    
    drawCurrent(theMonth);
    
    drawTemperatureScale(-10, 30, 8);
    drawLabels();

    mm.addFrame();  
    theMonth++;
    if (theMonth>12)
    {
      theMonth = theMonth%12;
      theYear++;
    }

  }
  else
  {
    println("Finishing movie!");
    mm.finish(); 
    noLoop();
  }
}

void drawLabels()
{
  fill(255);
  text(theYear, 15, 15); 
  text(months2text[theMonth-1], 15, 25);
  
  for (int i=0; i<12; i++)
  {
     radius = min(height/2, width/2)-25;
     angle = i/12.0 * TWO_PI + PI;
     px = cx + radius * cos(angle); 
     py = cy + radius * sin(angle);
     stroke(190);
     line(cx, cy, px, py);
     radius = min(height/2, width/2)-15;
     angle = i/12.0 * TWO_PI + PI;
     px = cx + radius * cos(angle); 
     py = cy + radius * sin(angle);
     
     stroke(255);
     text(months2text[i], px, py);   
  }
  
  fill(#4444FF);
  text("North", width-70, height - 60);

  fill(#FF4444);
  text("Tropics", width-70, height - 40);

  fill(#44FF44);
  text("South", width-70, height - 20);
}

void processNorth()
{
 if (lineNumberNorth < linesNorth.length) {
    String[] pieces = split(linesNorth[lineNumberNorth], '\t');
    if (pieces.length == 3) {
      int iYear = int(pieces[0]);
      int iMonth = int(pieces[1]);
      float temperature = float(pieces[2]);
      
      if (iYear == theYear && iMonth == theMonth)
      {
        drawPast(lineNumberNorth, MONTHS_IN_PAST, "NORTH");
        lineNumberNorth++;
      }
      else
      {
        lineNumberNorth = attemptUpdate(lineNumberNorth, "NORTH");
      }
      
    }    
  }
}

void processSouth()
{
 if (lineNumberSouth < linesSouth.length) {
    String[] pieces = split(linesSouth[lineNumberSouth], '\t');
    if (pieces.length == 3) {
      int iYear = int(pieces[0]);
      int iMonth = int(pieces[1]);
      float temperature = float(pieces[2]);
      
      if (iYear == theYear && iMonth == theMonth)
      {
        drawPast(lineNumberTropic, MONTHS_IN_PAST, "SOUTH");
        lineNumberSouth++;
      }
      else
      {
        lineNumberSouth = attemptUpdate(lineNumberSouth, "SOUTH");
      }
      
    }    
  }
}

void processTropic()
{
 if (lineNumberTropic < linesTropic.length) {
    String[] pieces = split(linesTropic[lineNumberTropic], '\t');
    if (pieces.length == 3) {
      int iYear = int(pieces[0]);
      int iMonth = int(pieces[1]);
      float temperature = float(pieces[2]);
      
      if (iYear == theYear && iMonth == theMonth)
      {
        drawPast(lineNumberTropic, MONTHS_IN_PAST, "TROPIC");
        lineNumberTropic++;
      }
      else
      {
        lineNumberTropic = attemptUpdate(lineNumberTropic, "TROPIC");
      }
      
    }    
  }
}

int attemptUpdate(int lineNumber, String dataset)
{
     
   String[] lines;
    
   if (dataset.equals("NORTH"))
   {
        lines = linesNorth;
   }
    else if (dataset.equals("SOUTH"))
    {
      lines = linesSouth;
    }
    else if (dataset.equals("TROPIC"))
{
  lines = linesTropic;
}   
  else
  {
    return lineNumber;
  }
    float timestampRef = theYear + (theMonth - 1)/12.0;
    float timestamp = 0.0;
    String[] pieces = split(lines[lineNumber], '\t');
    if (pieces.length == 3) {
      int iYear = int(pieces[0]);
      int iMonth = int(pieces[0]);
      timestamp = iYear + (iMonth - 1)/12.0;
    } 
    
    while (timestamp < timestampRef)
    {
      lineNumber++; 
      pieces = split(lines[lineNumber], '\t');
      if (pieces.length == 3) {
        int iYear = int(pieces[0]);
        int iMonth = int(pieces[0]);
        timestamp = iYear + (iMonth - 1)/12.0;  
      } 
    }
    return lineNumber;
  
}

void drawTemperatureScale(float tempMin, float tempMax, int steps)
{
  
  float delta = abs((tempMax - tempMin)/(1.0 * steps));
  ellipseMode(CENTER);
  
  for (int i=0; i < steps; i++)
  {  
      float radius = temp0radius + (tempMin + delta * i) * DEGREES_TO_PIXEL;
      
      fill(0, 0);
      stroke(#CCCCCC, 50);
      ellipse(cx, cy, 2*radius, 2*radius);
      
      stroke(#CCCCCC, 90);
      fill(200);
      text(int(tempMin + delta*i), cx + 2 + radius /* * cos(PI/4.0) */, cy /* * sin(PI/4.0)*/);
   }
  
}

void drawCurrent(int theMonth)
{
  radius = max(height/2, width/2)-100;
  angle = theMonth/12.0 * TWO_PI + PI;
  px = cx + radius * cos(angle); 
  py = cy + radius * sin(angle); 
  line(cx, cy, px, py);
}

void drawPast(int lineNumber, int window, String dataset )
{
  for (int i=0; i<window; i++)
  {
    if (0<lineNumber-i)
    {
      choosePastStroke(i, window, dataset);
      drawPastLine(lineNumber-i, lineNumber-i-1, dataset);
    }
  } 
}

void choosePastStroke(int delta, int window, String dataset)
{
  if (dataset.equals("NORTH"))
  {
        if(delta<12)
        {
          stroke(#0000FF, 255);
        }
        else
        {
          stroke(0, 0, 255 - (delta/(1.0*window))*150, 150 - delta/(1.0*window)*100);
        }
  }
  else if (dataset.equals("SOUTH"))
  {     if(delta<12)
        {
          stroke(#00FF00, 255);
        }
        else
        {
          stroke(0, 255 - (delta/(1.0*window))*150, 0, 150 - delta/(1.0*window)*100);
        }
  }
  else if (dataset.equals("TROPIC"))
  {
        if(delta<12)
        {
          stroke(#FF0000, 255);
        }
        else
        {
          stroke(255 - (delta/(1.0*window))*150, 0, 0, 150 - delta/(1.0*window)*100);
        }
  }
}

void drawPastLine(int start, int end, String dataset)
{
    int theMonthStart = 0;
    int theMonthEnd = 0;
    float temperatureStart = 0.0;
    float temperatureEnd = 0.0;
    
    String[] lines;
    
   if (dataset.equals("NORTH"))
   {
        lines = linesNorth;
   }
    else if (dataset.equals("SOUTH"))
    {
      lines = linesSouth;
    }
    else if (dataset.equals("TROPIC"))
{
  lines = linesTropic;
}   
  else
  {
    return;
  }
    String[] pieces = split(lines[start], '\t');
    if (pieces.length == 3) {
      theMonthStart = int(pieces[1]);
      temperatureStart = float(pieces[2]);
    }
    else
      return;
    
    pieces = split(lines[end], '\t');
    if (pieces.length == 3) {
      theMonthEnd = int(pieces[1]);
      temperatureEnd = float(pieces[2]);
    }
    else
      return;
    
    radius = temp0radius + temperatureStart * DEGREES_TO_PIXEL;
    angle = theMonthStart/12.0 * TWO_PI + PI;
    float pxStart = cx + radius * cos(angle); 
    float pyStart = cy + radius * sin(angle);
    
    radius = temp0radius + temperatureEnd * DEGREES_TO_PIXEL;
    angle = theMonthEnd/12.0 * TWO_PI + PI;
    float pxEnd = cx + radius * cos(angle); 
    float pyEnd = cy + radius * sin(angle); 
    
    line(pxStart, pyStart, pxEnd, pyEnd);    
}
