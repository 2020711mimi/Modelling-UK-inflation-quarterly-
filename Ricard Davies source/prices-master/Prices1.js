var NL = d3.locale ({
  "decimal": ".",
  "thousands": ",",
  "grouping": [3],
  "currency": ["£", ""],
  "dateTime": "%a %b %e %X %Y",
  "date": "%m/%d/%Y",
  "time": "%H:%M:%S",
  "periods": ["AM", "PM"],
  "days": ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
  "shortDays": ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
  "months": ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
  "shortMonths": ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
})


{
  "$schema": "https://vega.github.io/schema/vega-lite/v4.json",

  "description": "UK CPI data visualisation",

  "data": {
    "url": "https://raw.githubusercontent.com/RDeconomist/prices/master/db_item.csv"
  },

 "transform": [
    
    {"filter": {
        "field": "n_obs",
        "gt": 10000}  
          }
  ],

  "selection": {
    "paintbrush": {"type": "multi", "on": "mouseover", "nearest": true},
    "grid": {"type": "interval", "bind": "scales"},
    "division": {"type": "multi", "fields": ["division_sh"], "bind": "legend"}
  },

  "height":600,
  "width":600,

  "mark": "circle",

  "encoding": {
    "x": {
      "field": "p_rise_50",
      "type": "quantitative",
      "scale": {"zero": false},
      "title":"Inflation"
    },

    "y": {
      "field": "change1_sh",
      "type": "quantitative",
      "scale": {"zero": false},
      "title":"Volatility"
    },

    "color": {"field": "division_sh", "type": "nominal", "title":"Division", "scale": {"scheme": "turbo"}},

    "opacity": {"condition": {"selection": "division", "value": 0.5}, "value": 0},

    "size": {
      "condition": {"selection": "paintbrush", "value": 800},
      "field": "n_obs", "type": "nominal", "legend":null},

    "tooltip": [
      {"field": "name5", "type": "nominal", "title": "Item"},
      {"field": "startYear", "type": "quantitative", "title": "Start", "format": ".0f"},
      {"field": "endYear", "type": "quantitative", "title": "End", "format": ".0f"},
      {"field": "p50_s", "type": "quantitative", "title": "Start price (£)", "format": ".2f"},
      {"field": "p50_e", "type": "quantitative", "title": "End price (£)", "format": ".2f"},
      {"field": "p_rise_50", "type": "quantitative", "title": "Price rise", "format": ".0%"},
      {"field": "change1_sh", "type": "quantitative", "title": "Price changes", "format": ".0%"},
      {"field": "n_obs", "type": "quantitative", "title": "Prices", "format":","}        
    ]
  }
}