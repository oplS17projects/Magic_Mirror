# Magic Mirror

### Statement
Mirror, mirror on the wall...  

The Magic Mirror is a smart mirror that displays various items of interest, such as the weather within the reflective surface of the mirror.  The minimalist design allows information to be displayed on the edges of the mirror, where they're least likely to be  intrusive.  The colors were restricted to black and white in order to provide maximum visibility of the information to be displayed and minimize interference from the unused sections of the screen. The information on display is dynamically generated HTML that is created and served by Racket.   


### Analysis
Data abstraction is used throughout the project.  The intent was to simplify communicating between modules and between partners.  This allowed for exposing only the required functions while keeping others hidden away.  

Recursion is used throughout the project in building of lists, parsing of data and displaying the data within the mirror.


### External Technologies
A Raspberry Pi 3 Model B is the computer that behind the mirror.  Raspbian, the Debian-based Linux distribution has been loaded onto the Raspberry Pi due to its support of Racket. The mirror uses a Samsung 24 inch LED HDTV with a two-way acrylic mirrored sheet serving as the mirror. The mirror currently has six dedicated locations for placing "modules" with four of those locations in use.


### Web Communications
The weather information comes from OpenWeatherMap and the date and time information comes from a combination of both [OpenWeatherMap.org](http://openweathermap.org/) and [TimezoneDB.com](https://timezonedb.com/).  The Word Of The Day module uses webscraping to retrieve its information from [Dictionary.com](http://www.dictionary.com/wordoftheday/).


### Data Sets or other Source Materials
The external data sets are mentioned above.  The internal data sets including manipulating stored JSON hash-sets into lists for front-end consumption.  The preference system also stored its settings in JSON and parses them for the front-end.


### Deliverable and Demonstration
The end product as described in the statement section at the top of this document will be on display at the Tsongas Center at the [i2i Conference](https://www.uml.edu/conferences/i2i/).


### Evaluation of Results
The mirror was tested using several random locations for both weather and for time. Testing was done without network connectivity to simulate an outage.  The mirror should continue to function with cached data.  If no cached data is available, the affected module should return a null list.


### Architecture Diagram
![Architecture Diagram](https://github.com/oplS17projects/Magic_Mirror/blob/master/fp4-diagram-v2.png "Architecture Diagram")  

The OPL-FP.rkt file serves as the heart of the mirror.  Its job is to request data from the modules, generate a dynamic page and spawn the web server used to display the data within the mirror.   

The time.rkt module takes the latitude and longitude information from openweathermap.org and combines that with the request made from timezonedb.com in order to get the date and time for a given location by name. Without the latitude and longitude information from openweathermap, a subscription-based key would be necessary to obtain the data required.  

The weather.rkt will obtain either the current weather or forecast for a specified location.

The wotd.rkt module uses webscraping in order to obtain its data from dictionary.com. The coding goal of this module was to try webscraping. All other modules at this point make use of public APIs.

The preferences.rkt both reads and writes user preferences. In order to set preferences, the user would require a keyboard.  By entering (set-preferences) into the REPL, the user would be presented with a GUI that allows for simplified configuration.  The main application can ask for the preferences and have them returned as a list. If the mirror is run without the user setting preferences, a default preference file is created with Lowell, MA as the defaults for time and weather.  Reading preferences is done without a GUI.

## Schedule
### First Milestone (Sun Apr 9)
#### Steve Warren
The weather portion of the project was made functional. It allows for downloading current or forecast data and parsing JSON into a list format. Network connectivity and error-handling were put in place, as was file-caching.  

The file-caching serves as a source of data while Internet services are unavailable, and will prevent bombarding the service provider with multiple requests over a short period of time. The cache-file will be preferred to downloading a new data if the file is less than one hour old.

#### Steve Kim
Implement a basic GUI with static placement. As weather is the only source of data available at this point it will be the only thing shown.  

The GUI will have the weather hard-coded into the upper left and will unlikely have the final styling in place.


### Second Milestone (Sun Apr 16)
#### Steve Warren
Preferences were put into place as were additional modules. 


#### Steve Kim
The GUI will include other "modules" if provided and will allow displaying of modules on screen in a user-specified manner. This will most likely be done with a config file, but needs be researched further.  

The GUI styling will be finalized.

### Public Presentation: Fri Apr 28
As stated in the Deliverable and Demonstration section.  The Magic Mirror will be on display at the Tsongas Center 4/27/2017.

## Group Responsibilities
### Steve Warren @LordSpaghettiOs
Lead: Will work on the back-end components and hardware

### Steve "Wildcat" Kim @kimste2
Will work on the front-end components and GUI design
