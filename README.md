# Magic Mirror

### Statement
Mirror, mirror on the wall...  

The Magic Mirror is an IoT-based project that adds various pieces of information to a mirror. Using a double-sided mirror, an LCD TV, and a Raspberry Pi it's possible to provide information that appears in the glass of the mirror, while maintaining the reflectivity required of a mirror.  

In order to achieve the desired result, the output should remain minimalist and towards the sides and possibly the bottom of the mirror. The background should remain black to prevent as much light as possible from leaking through and the text should be white to provide maximum visibility. Graphics should also be as minimal as possible, or possibly excluded.  

We will attempt to bring a magic mirror in for presentation, but cost, weather, and transportation issues will need to be worked out. If we're unable to bring a mirror in for presentation, we'll still be able to present the project, though in a less fun manner.

### Analysis
While other techniques will almost certainly be brought in as we develop the project, these are what I know to be used at the time of this writeup.  

While working on FP3, we've found the need for recursion in parsing data sets.  These sets were not including the FP3 submission, but will be reintroduced in the coming weeks.  

Designing with data abstraction in mind has also made it easier for us to interact with.  For instance, within FP3 I've constructed two functions for getting and parsing weather.  The implementation and storage information are hidden away.  This allowed for multiple changes to the code, filesystem, and even different data streams without having to force my partner to alter his design.

### External Technologies
#### Raspberry Pi
- The Raspberry Pi 3 Model B will be the computer that drives the mirror. Raspbian, the Debian-based Linux distribution that I've loaded onto the Raspberry Pi supports Racket.

#### Web Communications
- The weather "module" currently connects to, and downloads data from [OpenWeatherMap.org](http://openweathermap.org/).  This data will be displayed via Racket webserver.  Other modules and functionality are planned.

### Data Sets or other Source Materials
The project currently downloads and parses JSON data. The information is converted into a complex list structure which than can be more easily read and worked with while populating the user interface.  

Other data sets are currently planned and will use a similar approach.

### Deliverable and Demonstration
The final product will display a minimalist output of information chosen by the end user on an internal webserver.  Without the mirror, it will simply be a display of the parsed data at the specified locations.


### Evaluation of Results
Results will be determined by unit testing, file-caching, and error handling. Success will be claimed when all tests have passed and the mirror is able to display information, either current or cached depending on the state of the network.


## Architecture Diagram
![FP4 Diagram](fp4_diagram-v2.png "FP4 Diagram")  

The front-end is responsible for requesting the current weather and/or forecast data for a specified city and country/state. It parses these results and displays them on the mirror.  

The weather.rkt recieves the request from the front-end.  It determines if there's a new enough cache-file stored locally.  If there is, it parses that into a list and returns that to the front-end.  If the cache file is deemed too old or is not available, it requests a new file from OpenWeatherMap. If the network is unavailable and the cache is missing a null list is returned.

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
Its our intention to build and show a magic mirror for presentation, but there are several issues that we'll need to resolve first.

## Group Responsibilities
### Steve Warren @LordSpaghettiOs
Lead: Will work on the backend components and hardware

### Steve "Wildcat" Kim @kimste2
Will work on the frontend components and GUI design