$.getJSON("http://api.twitter.com/1/statuses/user_timeline.json?screen_name=dying_sphynx&count=2&callback=?",
          function (tweets) {
              var date = new Date(tweets[0].created_at);
              var url = "http://twitter.com/dying_sphynx/status/" + tweets[0].id;
              var container = $("#last_tweet");
              container.html($("<time>")
                               .attr("datetime", date.toISOString())
                               .html(date.toDateString().substr(3)));
              container.append(" ");
              container.append($("<a>").attr("href", url).html(tweets[0].text));
          });

$.getJSON("http://api.twitter.com/1/users/show.json?screen_name=dying_sphynx&callback=?",
          function (info) {
              $("#twitter_readers").text(info.followers_count);
          });

/* commented as it has ceased to work for now. TODO: figure out what's
   wrong with Last.fm webservices.

// Last.FM top artists
var user = "dying_sphynx";
var api_code = "c259d1e967ce6249210ef9f230f1243d";
//var method = "user.getTopArtists";
var method = "user.getWeeklyArtistChart";
var limit = 5;
var url = "http://ws.audioscrobbler.com/2.0/?method=" + method
      + "&api_key=" + api_code + "&user=" + user
      + "&limit=" + limit + "&format=json&callback=?";

$.getJSON(url,
          function (resp) {
              var top_artists = resp.weeklyartistchart .artist;
              var top_names = jQuery.map(top_artists, function (a) { return a.name; });
              $("#last-song").text(top_names.join(", "));
          });
*/
