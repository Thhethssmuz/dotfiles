#!/usr/bin/env node
'use strict';

const http     = require('http').request;
const https    = require('https').request;
const entities = {'&AElig;':'%C6','&Aacute;':'%C1','&Acirc;':'%C2','&Agrave;':'%C0','&Alpha;':'%u0391','&Aring;':'%C5','&Atilde;':'%C3','&Auml;':'%C4','&Beta;':'%u0392','&Ccedil;':'%C7','&Chi;':'%u03A7','&Dagger;':'%u2021','&Delta;':'%u0394','&ETH;':'%D0','&Eacute;':'%C9','&Ecirc;':'%CA','&Egrave;':'%C8','&Epsilon;':'%u0395','&Eta;':'%u0397','&Euml;':'%CB','&Gamma;':'%u0393','&Iacute;':'%CD','&Icirc;':'%CE','&Igrave;':'%CC','&Iota;':'%u0399','&Iuml;':'%CF','&Kappa;':'%u039A','&Lambda;':'%u039B','&Mu;':'%u039C','&Ntilde;':'%D1','&Nu;':'%u039D','&OElig;':'%u0152','&Oacute;':'%D3','&Ocirc;':'%D4','&Ograve;':'%D2','&Omega;':'%u03A9','&Omicron;':'%u039F','&Oslash;':'%D8','&Otilde;':'%D5','&Ouml;':'%D6','&Phi;':'%u03A6','&Pi;':'%u03A0','&Prime;':'%u2033','&Psi;':'%u03A8','&Rho;':'%u03A1','&Scaron;':'%u0160','&Sigma;':'%u03A3','&THORN;':'%DE','&Tau;':'%u03A4','&Theta;':'%u0398','&Uacute;':'%DA','&Ucirc;':'%DB','&Ugrave;':'%D9','&Upsilon;':'%u03A5','&Uuml;':'%DC','&Xi;':'%u039E','&Yacute;':'%DD','&Yuml;':'%u0178','&Zeta;':'%u0396','&aacute;':'%E1','&acirc;':'%E2','&acute;':'%B4','&aelig;':'%E6','&agrave;':'%E0','&alpha;':'%u03B1','&amp;':'%26','&and;':'%u2227','&ang;':'%u2220','&apos;':'%27','&aring;':'%E5','&asymp;':'%u2248','&atilde;':'%E3','&auml;':'%E4','&bdquo;':'%u201E','&beta;':'%u03B2','&brvbar;':'%A6','&bull;':'%u2022','&cap;':'%u2229','&ccedil;':'%E7','&cedil;':'%B8','&cent;':'%A2','&chi;':'%u03C7','&circ;':'%u02C6','&clubs;':'%u2663','&cong;':'%u2245','&copy;':'%A9','&crarr;':'%u21B5','&cup;':'%u222A','&curren;':'%A4','&dagger;':'%u2020','&darr;':'%u2193','&deg;':'%B0','&delta;':'%u03B4','&diams;':'%u2666','&divide;':'%F7','&eacute;':'%E9','&ecirc;':'%EA','&egrave;':'%E8','&empty;':'%u2205','&emsp;':'%u2003','&ensp;':'%u2002','&epsilon;':'%u03B5','&equiv;':'%u2261','&eta;':'%u03B7','&eth;':'%F0','&euml;':'%EB','&euro;':'%u20AC','&exist;':'%u2203','&fnof;':'%u0192','&forall;':'%u2200','&frac12;':'%BD','&frac14;':'%BC','&frac34;':'%BE','&gamma;':'%u03B3','&ge;':'%u2265','&gt;':'%3E','&harr;':'%u2194','&hearts;':'%u2665','&hellip;':'%u2026','&iacute;':'%ED','&icirc;':'%EE','&iexcl;':'%A1','&igrave;':'%EC','&infin;':'%u221E','&int;':'%u222B','&iota;':'%u03B9','&iquest;':'%BF','&isin;':'%u2208','&iuml;':'%EF','&kappa;':'%u03BA','&lambda;':'%u03BB','&laquo;':'%AB','&larr;':'%u2190','&lceil;':'%u2308','&ldquo;':'%u201C','&le;':'%u2264','&lfloor;':'%u230A','&lowast;':'%u2217','&loz;':'%u25CA','&lrm;':'%u200E','&lsaquo;':'%u2039','&lsquo;':'%u2018','&lt;':'%3C','&macr;':'%AF','&mdash;':'%u2014','&micro;':'%B5','&middot;':'%B7','&minus;':'%u2212','&mu;':'%u03BC','&nabla;':'%u2207','&ndash;':'%u2013','&ne;':'%u2260','&ni;':'%u220B','&not;':'%AC','&notin;':'%u2209','&nsub;':'%u2284','&ntilde;':'%F1','&nu;':'%u03BD','&oacute;':'%F3','&ocirc;':'%F4','&oelig;':'%u0153','&ograve;':'%F2','&oline;':'%u203E','&omega;':'%u03C9','&omicron;':'%u03BF','&oplus;':'%u2295','&or;':'%u2228','&ordf;':'%AA','&ordm;':'%BA','&oslash;':'%F8','&otilde;':'%F5','&otimes;':'%u2297','&ouml;':'%F6','&para;':'%B6','&part;':'%u2202','&permil;':'%u2030','&perp;':'%u22A5','&phi;':'%u03C6','&pi;':'%u03C0','&piv;':'%u03D6','&plusmn;':'%B1','&pound;':'%A3','&prime;':'%u2032','&prod;':'%u220F','&prop;':'%u221D','&psi;':'%u03C8','&quot;':'%22','&radic;':'%u221A','&raquo;':'%BB','&rarr;':'%u2192','&rceil;':'%u2309','&rdquo;':'%u201D','&reg;':'%AE','&rfloor;':'%u230B','&rho;':'%u03C1','&rlm;':'%u200F','&rsaquo;':'%u203A','&rsquo;':'%u2019','&sbquo;':'%u201A','&scaron;':'%u0161','&sdot;':'%u22C5','&sect;':'%A7','&shy;':'%AD','&sigma;':'%u03C3','&sigmaf;':'%u03C2','&sim;':'%u223C','&spades;':'%u2660','&sub;':'%u2282','&sube;':'%u2286','&sum;':'%u2211','&sup1;':'%B9','&sup2;':'%B2','&sup3;':'%B3','&sup;':'%u2283','&supe;':'%u2287','&szlig;':'%DF','&tau;':'%u03C4','&there4;':'%u2234','&theta;':'%u03B8','&thetasym;':'%u03D1','&thinsp;':'%u2009','&thorn;':'%FE','&tilde;':'%u02DC','&times;':'%D7','&trade;':'%u2122','&uacute;':'%FA','&uarr;':'%u2191','&ucirc;':'%FB','&ugrave;':'%F9','&uml;':'%A8','&upsih;':'%u03D2','&upsilon;':'%u03C5','&uuml;':'%FC','&xi;':'%u03BE','&yacute;':'%FD','&yen;':'%A5','&yuml;':'%FF','&zeta;':'%u03B6','&zwj;':'%u200D','&zwnj;':'%u200C'};

const COLUMNS  = parseInt(process.argv[2], 10);
const FUNCTION = process.argv[3];
const ARGUMENTS= process.argv.slice(4);

let decode = str => str
  .replace(/(&\w+;)/g, x => entities[x] || '')
  .replace(/&#(\d+);/g, (x,c) => String.fromCharCode(c))
  .replace(/&#x([0-9A-F]+);/gi, (x,c) => String.fromCharCode(parseInt(c, 16)));

let wrap = function (indent, text) {
  let l = COLUMNS - indent;

  let lines = text.split(' ').reduce((xs,x) => {
    while (x.length > l) {
      xs.unshift(x.slice(0,l));
      x = x.slice(l);
    }
    if (xs[0].length + x.length + 1 > l)
      xs.unshift(x);
    else
      xs[0] += ' ' + x;
    return xs;
  }, ['']).reverse();

  if (lines[0].length > 0)
    lines[0] = lines[0].slice(1);
  if (lines[0].length === 0 && lines.length > 0)
    lines.shift();

  return lines
    .map(x => Array(indent+1).join(' ') + x + Array(l-x.length+1).join(' '))
    .join('\n');
};

exports.glosbe = function (from, dest, phrase) {
  let url = 'https://glosbe.com/gapi/translate?from=' + from + '&dest=' + dest +
            '&format=json&phrase=' + encodeURIComponent(phrase);

  https(url, res => {
    let data = '';
    res.on('data', d => data += d);
    res.on('end', () => {
      let rs = JSON.parse(data).tuc.filter(x => x.phrase).map(x => {
        return {
          text   : decode(x.phrase.text),
          meaning: new Set((x.meanings || []).map(x => decode(x.text)))
        };
      });

      console.log();
      console.log(wrap(2, rs.map(x => x.text).join(', ')));

      rs.filter(x => x.meaning.size).forEach(x => {
        console.log('\n%s\n', x.text);
        let i = 0;
        x.meaning.forEach(x => {
          console.log('%s. %s', ('  '+(i+=1)).slice(-3), wrap(5, x).slice(5));
        });
      });
    });
  }).end();
};

exports.dictionaryapi = function (word) {
  let url = 'http://dictionaryapi.net/api/definition/' + word;

  http(url, res => {
    let data = '';
    res.on('data', d => data += d);
    res.on('end', () => {
      JSON.parse(data).forEach(x => {

        console.log(x.Word + ' (' + x.PartOfSpeech + ')');
        console.log();

        if (x.Forms.length) {
          console.log(wrap(2, x.Forms.join(', ')));
          console.log();
        }

        if (x.Definitions.length) {
          x.Definitions.forEach((x,i) => {
            console.log('%s. %s', ('  '+(i+1)).slice(-3), wrap(5, x).slice(5));
          });
          console.log();
        }

      });
    });
  }).end();
};

exports[FUNCTION].apply(null, ARGUMENTS);
