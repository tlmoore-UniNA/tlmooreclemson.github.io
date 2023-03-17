var webPage = require('webpage');
var url ='https://www.scopus.com/authid/detail.uri?authorId=36542652400';
var fs = require('fs'); 
var page = webPage.create();
var system = require('system');

page.settings.userAgent = 'Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)';

page.open(url, function (status) {
        setTimeout(function() {
               fs.write('rendered_page.html', page.content, 'w');
            phantom.exit();
    }, 2500);
});
