const url = "http://localhost:8000/examples/simple-nightwatch/index.html";
const textInputSelector = ".elm-datepicker--input";
const topLeftDaySelector = ".elm-datepicker--row:first-child .elm-datepicker--day:first-child";
const errorMsgSelector = "#error"
const openBtn = "#openpickerbtn"
const closeBtn = "#closepickerbtn"
const nextMonthBtn = ".elm-datepicker--next"
const prevMonthBtn = ".elm-datepicker--prev"
const monthDiv = ".elm-datepicker--month"

const defaultWait = 1000;

module.exports = {

    'Next month button should navigate to next month': function(client) {
        client.url(url);
        client.expect.element(openBtn).to.be.present.before(defaultWait);
        client.click(openBtn);
        client.expect.element(nextMonthBtn).to.be.present.before(defaultWait);
        client.click(nextMonthBtn);
        client.expect.element(monthDiv).text.to.contain("August");
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/07/27").before(defaultWait);

    },

    'Prev month button should navigate to prev month': function(client) {
        client.url(url);
        client.expect.element(openBtn).to.be.present.before(defaultWait);
        client.click(openBtn);
        client.expect.element(prevMonthBtn).to.be.present.before(defaultWait);
        client.click(prevMonthBtn);
        client.expect.element(monthDiv).text.to.contain("June");
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/06/01").before(defaultWait);

    },

    'When selecting a date with the mouse, it should appear in the text input' : function (client) {
        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/06/29").before(defaultWait);
        client.end();
    },

    'When entering text, and then selecting a date with the mouse, the selected date should appear in the text input' : function (client) {
        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        client.sendKeys(textInputSelector, "1980-01-01");
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/06/29").before(defaultWait);
        client.end();
    },

    'When entering the text of a valid date, then pressing the ENTER key, the entered date should appear in the date picker' : function (client) {
        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        client.sendKeys(textInputSelector, "1980-01-01");
        client.sendKeys(textInputSelector, client.Keys.ENTER);
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.expect.element(".elm-datepicker--row:first-child .elm-datepicker--day:nth-child(3)")
            .to.have.attribute('class').which.contains('elm-datepicker--picked');
        client.expect.element("h1").text.to.equal("Jan 01, 1980");

        // now we click on another value, to make sure the input is updated
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1979/12/30").before(defaultWait);
        client.expect.element("h1").text.to.equal("Dec 30, 1979");
        client.end();
    },

    'Characters should not be dropped when entering text quickly' : function (client) {

        const longTextExample = "The quick brown fox jumped over the lazy dog";

        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        client.sendKeys(textInputSelector, longTextExample);
        client.expect.element(textInputSelector).value.to.equal(longTextExample).before(defaultWait);
        client.end();
    },

    'Manually entered invalid dates should be interceptable' : function (client) {
        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        client.sendKeys(textInputSelector, "1980-01-01a");
        client.sendKeys(textInputSelector, client.Keys.ENTER);
        client.expect.element(errorMsgSelector).to.be.present.before(defaultWait);
        client.expect.element(errorMsgSelector).text.to.contain("Parser error: Expected a date only").before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/06/29").before(defaultWait);
        client.expect.element(errorMsgSelector).to.not.be.present.before(defaultWait);
        client.end();
    },

    'Manually entered disabled dates should be interceptable' : function (client) {
        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        client.sendKeys(textInputSelector, "1980-01-02");
        client.sendKeys(textInputSelector, client.Keys.ENTER);
        client.expect.element(errorMsgSelector).to.be.present.before(defaultWait);
        client.expect.element(errorMsgSelector).text.to.contain("Date disabled: 1980-01-02").before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/06/29").before(defaultWait);
        client.expect.element(errorMsgSelector).to.not.be.present.before(defaultWait);
        client.end();
    },

    'Open and Close Msg should open and the Picker' : function (client) {
        client.url(url);
        client.expect.element(openBtn).to.be.present.before(defaultWait);
        client.click(openBtn);
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.click(closeBtn);
        client.expect.element(topLeftDaySelector).to.not.be.present.before(defaultWait);
        client.click(openBtn);
        client.expect.element(topLeftDaySelector).to.be.present.before(defaultWait);
        client.click(topLeftDaySelector);
        client.expect.element(textInputSelector).value.to.equal("1969/06/29").before(defaultWait);
        client.expect.element(errorMsgSelector).to.not.be.present.before(defaultWait);
        client.end();
    },

    'Manually entered text should not be cleared when the view re-renders' : function (client) {

        client.url(url);
        client.expect.element(textInputSelector).to.be.present.before(defaultWait);
        client.click(textInputSelector);
        slowlySendKeys(client, textInputSelector, "testing");

        // the SimpleNightwatch.elm app has a NoOp message that is triggered after 2
        // seconds. The NoOp causes a re-render of the app. This should not
        // clear the manually entered text.
        client.pause(3000);
        client.expect.element(textInputSelector).value.to.equal("testing").before(defaultWait);
        client.end();
    },


};


function slowlySendKeys(client, selector, text) {
    for (i in text) {
        client.sendKeys(selector, text[i]);
        client.pause(50);
    }
}
