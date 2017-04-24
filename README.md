# Magic Mirror

### Statement
Mirror, mirror on the wall...  

The Magic Mirror was an IoT-based project that display various pieces of information to a mirror.  The output was minimalistic and displayed on the lateral, top or bottom sides.  The background of the mirror was kept black which minimizes the amount of light leaking through.  All text and images are white to allow for maximum visibility due to ambient light.   

The Raspberry Pi 3 Model B was the computer that powers the mirror.  Raspbian, the Debian-based Linux distribution has been loaded onto the Raspberry Pi because it supports Racket.  A Samsung 24 inch LED HDTV was the screen that will serve as the projector.  The TV has a two-way mirror mounted on top of it.  Our magic mirror has four different modules.  An end user can configure global weather information, global time, a word of the day and a quote of the day.  



### Analysis
Designing with data abstraction in mind has made it easier for us to interact with each other’s code.  Implementation and storage details have not been actively discussed.  This allows for either of us to change code, filesystems and data streams without forcing the other to alter his design.  Linear recursion was used in a lot of our parsing, searching and filtering functions.   

### External Technologies
The Raspberry Pi 3 Model B was the computer that powers the mirror.  Raspbian, the Debian-based Linux distribution has been loaded onto the Raspberry Pi because it supports Racket.  A Samsung 24 inch LED HDTV was the screen that will serve as the projector.  The TV has a two-way mirror mounted on top of it.  Our magic mirror has four different modules.  An end user can configure global weather information, global time, a word of the day and a quote of the day.

#### Web Communications
- Any weather information was obtained using a free API key from OpenWeatherMap.  Time information was obtained both from OpenWeatherMap and Timezonedb.  The word of the day information was directly from Dictionary.com.


### Data Sets or other Source Materials
The web communication information was converted into a complex list structure to make reading and working with the data easier.  Both global weather and global time information are procured by downloading and parsing JSON data.  The word of the day was webscraped directly from Dictionary.com.  The quote of the day was created by reading from a local text document and converting that data into a list.
### Deliverable and Demonstration
The final product will be the magic mirror itself which can be customized as an end user sees fit.

### Evaluation of Results
The working mirror will be the ultimate test of our success. 

## Architecture Diagram
![FP4 Diagram](https://github.com/oplS17projects/Magic_Mirror/blob/master/fp4-diagram-v2.png "FP4 Diagram")  

The front-end.rkt file is the heart of our mirror because it requests information from other .rkt files.  It will parse the resulting information and display them on the mirror.   

For example  if the weather.rkt recieves the request from the front-end.  It determines if there's a new enough cache-file stored locally.  If there is, it parses that into a list and returns that to the front-end.  If the cache file is deemed too old or is not available, it requests a new file from OpenWeatherMap. If the network is unavailable and the cache is missing a null list is returned.

## Schedule
### First Milestone (Sun Apr 9)
#### Steve Warren
The weather portion of the project will be functional. It will allow downloading of current or forecast data and parsing JSON into a list format. Network connectivity error-handling will be in place, as will file-caching.  

The file-caching will serve as a source of data while internet services are unavailable, and will prevent bombarding the serive provider with multiple requests over a short period of time. At the time of this writing, the cache-file will be prefered to downloading new data if the file is less than one hour old.

#### Steve Kim
Implement a basic GUI with static placement. As weather is the only source of data available at this point it will be the only thing shown.  

The GUI will have the weather hard-coded into the upper left and will unlikely have the final styling in place.

### Second Milestone (Sun Apr 16)
#### Steve Warren
Unit tests will be in place. Additional backend "modules" are being discussed.

#### Steve Kim
The GUI will include other "modules" if provided and will allow displaying of modules on screen in a user-specified manner. This will most likely be done with a config file, but needs be researched further.  

The GUI styling will be finalized.

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
Its our intention to build and show a magic mirror for our presentation on Friday April 28th.  We will be presenting at the Tsongas Center.  

## Group Responsibilities
### Steve Warren @LordSpaghettiOs
Lead: Will work on the backend components and hardware

### Steve "Wildcat" Kim @kimste2
Will work on the frontend components and GUI design
