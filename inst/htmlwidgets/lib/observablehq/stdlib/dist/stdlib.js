// @observablehq/stdlib v2.0.8 Copyright 2019 Observable, Inc.
!function(e,t){"object"==typeof exports&&"undefined"!=typeof module?t(exports):"function"==typeof define&&define.amd?define(["exports"],t):t(e.observablehq=e.observablehq||{})}(this,function(e){"use strict";function t(e){return function(){return e}}var n={math:"http://www.w3.org/1998/Math/MathML",svg:"http://www.w3.org/2000/svg",xhtml:"http://www.w3.org/1999/xhtml",xlink:"http://www.w3.org/1999/xlink",xml:"http://www.w3.org/XML/1998/namespace",xmlns:"http://www.w3.org/2000/xmlns/"};var r=0;function o(e){this.id=e,this.href=window.location.href+"#"+e}o.prototype.toString=function(){return"url("+this.href+")"};var i={canvas:function(e,t){var n=document.createElement("canvas");return n.width=e,n.height=t,n},context2d:function(e,t,n){null==n&&(n=devicePixelRatio);var r=document.createElement("canvas");r.width=e*n,r.height=t*n,r.style.width=e+"px";var o=r.getContext("2d");return o.scale(n,n),o},download:function(e,t="untitled",n="Save"){const r=document.createElement("a"),o=r.appendChild(document.createElement("button"));async function i(){await new Promise(requestAnimationFrame),URL.revokeObjectURL(r.href),r.removeAttribute("href"),o.textContent=n,o.disabled=!1}return o.textContent=n,r.download=t,r.onclick=(async t=>{if(o.disabled=!0,r.href)return i();o.textContent="Saving…";try{const t=await("function"==typeof e?e():e);o.textContent="Download",r.href=URL.createObjectURL(t)}catch(e){o.textContent=n}if(t.eventPhase)return i();o.disabled=!1}),r},element:function(e,t){var r,o=e+="",i=o.indexOf(":");i>=0&&"xmlns"!==(o=e.slice(0,i))&&(e=e.slice(i+1));var u=n.hasOwnProperty(o)?document.createElementNS(n[o],e):document.createElement(e);if(t)for(var a in t)i=(o=a).indexOf(":"),r=t[a],i>=0&&"xmlns"!==(o=a.slice(0,i))&&(a=a.slice(i+1)),n.hasOwnProperty(o)?u.setAttributeNS(n[o],a,r):u.setAttribute(a,r);return u},input:function(e){var t=document.createElement("input");return null!=e&&(t.type=e),t},range:function(e,t,n){1===arguments.length&&(t=e,e=null);var r=document.createElement("input");return r.min=e=null==e?0:+e,r.max=t=null==t?1:+t,r.step=null==n?"any":n=+n,r.type="range",r},select:function(e){var t=document.createElement("select");return Array.prototype.forEach.call(e,function(e){var n=document.createElement("option");n.value=n.textContent=e,t.appendChild(n)}),t},svg:function(e,t){var n=document.createElementNS("http://www.w3.org/2000/svg","svg");return n.setAttribute("viewBox",[0,0,e,t]),n.setAttribute("width",e),n.setAttribute("height",t),n},text:function(e){return document.createTextNode(e)},uid:function(e){return new o("O-"+(null==e?"":e+"-")+ ++r)}};var u={buffer:function(e){return new Promise(function(t,n){var r=new FileReader;r.onload=function(){t(r.result)},r.onerror=n,r.readAsArrayBuffer(e)})},text:function(e){return new Promise(function(t,n){var r=new FileReader;r.onload=function(){t(r.result)},r.onerror=n,r.readAsText(e)})},url:function(e){return new Promise(function(t,n){var r=new FileReader;r.onload=function(){t(r.result)},r.onerror=n,r.readAsDataURL(e)})}};function a(){return this}function l(e,t){let n=!1;return{[Symbol.iterator]:a,next:()=>n?{done:!0}:(n=!0,{done:!1,value:e}),return:()=>(n=!0,t(e),{done:!0}),throw:()=>({done:n=!0})}}function c(e){let t,n,r=!1;const o=e(function(e){n?(n(e),n=null):r=!0;return t=e});return{[Symbol.iterator]:a,throw:()=>({done:!0}),return:()=>(null!=o&&o(),{done:!0}),next:function(){return{done:!1,value:r?(r=!1,Promise.resolve(t)):new Promise(e=>n=e)}}}}function s(e){switch(e.type){case"range":case"number":return e.valueAsNumber;case"date":return e.valueAsDate;case"checkbox":return e.checked;case"file":return e.multiple?e.files:e.files[0];default:return e.value}}var d={disposable:l,filter:function*(e,t){for(var n,r=-1;!(n=e.next()).done;)t(n.value,++r)&&(yield n.value)},input:function(e){return c(function(t){var n=function(e){switch(e.type){case"button":case"submit":case"checkbox":return"click";case"file":return"change";default:return"input"}}(e),r=s(e);function o(){t(s(e))}return e.addEventListener(n,o),void 0!==r&&t(r),function(){e.removeEventListener(n,o)}})},map:function*(e,t){for(var n,r=-1;!(n=e.next()).done;)yield t(n.value,++r)},observe:c,queue:function(e){let t;const n=[],r=e(function(e){return n.push(e),t&&(t(n.shift()),t=null),e});return{[Symbol.iterator]:a,throw:()=>({done:!0}),return:()=>(null!=r&&r(),{done:!0}),next:function(){return{done:!1,value:n.length?Promise.resolve(n.shift()):new Promise(e=>t=e)}}}},range:function*(e,t,n){e=+e,t=+t,n=(o=arguments.length)<2?(t=e,e=0,1):o<3?1:+n;for(var r=-1,o=0|Math.max(0,Math.ceil((t-e)/n));++r<o;)yield e+r*n},valueAt:function(e,t){if(!(!isFinite(t=+t)||t<0||t!=t|0))for(var n,r=-1;!(n=e.next()).done;)if(++r===t)return n.value},worker:function(e){const t=URL.createObjectURL(new Blob([e],{type:"text/javascript"})),n=new Worker(t);return l(n,()=>{n.terminate(),URL.revokeObjectURL(t)})}};function f(e,t){return function(n){var r,o,i,u,a,l,c,s,d=n[0],f=[],m=null,h=-1;for(a=1,l=arguments.length;a<l;++a){if((r=arguments[a])instanceof Node)f[++h]=r,d+="\x3c!--o:"+h+"--\x3e";else if(Array.isArray(r)){for(c=0,s=r.length;c<s;++c)(o=r[c])instanceof Node?(null===m&&(f[++h]=m=document.createDocumentFragment(),d+="\x3c!--o:"+h+"--\x3e"),m.appendChild(o)):(m=null,d+=o);m=null}else d+=r;d+=n[a]}if(m=e(d),++h>0){for(i=new Array(h),u=document.createTreeWalker(m,NodeFilter.SHOW_COMMENT,null,!1);u.nextNode();)o=u.currentNode,/^o:/.test(o.nodeValue)&&(i[+o.nodeValue.slice(2)]=o);for(a=0;a<h;++a)(o=i[a])&&o.parentNode.replaceChild(f[a],o)}return 1===m.childNodes.length?m.removeChild(m.firstChild):11===m.nodeType?((o=t()).appendChild(m),o):m}}var m=f(function(e){var t=document.createElement("template");return t.innerHTML=e.trim(),document.importNode(t.content,!0)},function(){return document.createElement("span")});function h(e){return function(){return e("marked@0.3.12/marked.min.js").then(function(t){return f(function(n){var r=document.createElement("div");r.innerHTML=t(n,{langPrefix:""}).trim();var o=r.querySelectorAll("pre code[class]");return o.length>0&&e("@observablehq/highlight.js@1.1.1/highlight.min.js").then(function(e){o.forEach(function(t){e.highlightBlock(t),t.parentNode.classList.add("observablehq--md-pre")})}),r},function(){return document.createElement("div")})})}}function v(e){let t;Object.defineProperties(this,{generator:{value:c(e=>void(t=e))},value:{get:()=>e,set:n=>t(e=n)}}),void 0!==e&&t(e)}function*w(){for(;;)yield Date.now()}var p=new Map;function b(e,n){var r;return(r=p.get(e=+e))?r.then(t(n)):(r=Date.now())>=e?Promise.resolve(n):function(e,t){var n=new Promise(function(n){p.delete(t);var r=t-e;if(!(r>0))throw new Error("invalid time");if(r>2147483647)throw new Error("too long to wait");setTimeout(n,r)});return p.set(t,n),n}(r,e).then(t(n))}var g={delay:function(e,t){return new Promise(function(n){setTimeout(function(){n(t)},e)})},tick:function(e,t){return b(Math.ceil((Date.now()+1)/e)*e,t)},when:b};function x(e,t){if(/^(\w+:)|\/\//i.test(e))return e;if(/^[.]{0,2}\//i.test(e))return new URL(e,null==t?location:t).href;if(!e.length||/^[\s._]/.test(e)||/\s$/.test(e))throw new Error("illegal name");return"https://unpkg.com/"+e}const y=new Map,E=[],P=E.map,k=E.some,j=E.hasOwnProperty,L="https://cdn.jsdelivr.net/npm/",$=/^((?:@[^\/@]+\/)?[^\/@]+)(?:@([^\/]+))?(?:\/(.*))?$/,A=/^\d+\.\d+\.\d+(-[\w-.+]+)?$/,C=/\.[^\/]*$/,M=["unpkg","jsdelivr","browser","main"];class RequireError extends Error{constructor(e){super(e)}}function N(e){const t=$.exec(e);return t&&{name:t[1],version:t[2],path:t[3]}}function O(e){const t=`${L}${e.name}${e.version?`@${e.version}`:""}/package.json`;let n=y.get(t);return n||y.set(t,n=fetch(t).then(e=>{if(!e.ok)throw new RequireError("unable to load package.json");return e.redirected&&!y.has(e.url)&&y.set(e.url,n),e.json()})),n}RequireError.prototype.name=RequireError.name;const R=S(async function(e,t){if(e.startsWith(L)&&(e=e.substring(L.length)),/^(\w+:)|\/\//i.test(e))return e;if(/^[.]{0,2}\//i.test(e))return new URL(e,null==t?location:t).href;if(!e.length||/^[\s._]/.test(e)||/\s$/.test(e))throw new RequireError("illegal name");const n=N(e);if(!n)return`${L}${e}`;if(!n.version&&null!=t&&t.startsWith(L)){const e=await O(N(t.substring(L.length)));n.version=e.dependencies&&e.dependencies[n.name]||e.peerDependencies&&e.peerDependencies[n.name]}if(n.path&&!C.test(n.path)&&(n.path+=".js"),n.path&&n.version&&A.test(n.version))return`${L}${n.name}@${n.version}/${n.path}`;const r=await O(n);return`${L}${r.name}@${r.version}/${n.path||function(e){for(const t of M){const n=e[t];if("string"==typeof n)return C.test(n)?n:`${n}.js`}}(r)||"index.js"}`});function S(e){const t=new Map,n=o(null);function r(e){if("string"!=typeof e)return e;let n=t.get(e);return n||t.set(e,n=new Promise((t,n)=>{const r=document.createElement("script");r.onload=(()=>{try{t(E.pop()(o(e)))}catch(e){n(new RequireError("invalid module"))}r.remove()}),r.onerror=(()=>{n(new RequireError("unable to load module")),r.remove()}),r.async=!0,r.src=e,window.define=T,document.head.appendChild(r)})),n}function o(t){return n=>Promise.resolve(e(n,t)).then(r)}function i(e){return arguments.length>1?Promise.all(P.call(arguments,n)).then(U):n(e)}return i.alias=function(t){return S((n,r)=>n in t&&(r=null,"string"!=typeof(n=t[n]))?n:e(n,r))},i.resolve=e,i}function U(e){const t={};for(const n of e)for(const e in n)j.call(n,e)&&(null==n[e]?Object.defineProperty(t,e,{get:q(n,e)}):t[e]=n[e]);return t}function q(e,t){return()=>e[t]}function D(e){return e+""=="exports"}function T(e,t,n){const r=arguments.length;r<2?(n=e,t=[]):r<3&&(n=t,t="string"==typeof e?[]:e),E.push(k.call(t,D)?e=>{const r={};return Promise.all(P.call(t,t=>D(t+="")?r:e(t))).then(e=>(n.apply(null,e),r))}:e=>Promise.all(P.call(t,e)).then(e=>"function"==typeof n?n.apply(null,e):n))}function F(e){return null==e?R:S(e)}T.amd={};var W=f(function(e){var t=document.createElementNS("http://www.w3.org/2000/svg","g");return t.innerHTML=e.trim(),t},function(){return document.createElementNS("http://www.w3.org/2000/svg","g")}),_=String.raw;function B(e){return new Promise(function(t,n){var r=document.createElement("link");r.rel="stylesheet",r.href=e,r.onerror=n,r.onload=t,document.head.appendChild(r)})}function H(e){return function(){return Promise.all([e("@observablehq/katex@0.10.1/dist/katex.min.js"),e.resolve("@observablehq/katex@0.10.1/dist/katex.min.css").then(B)]).then(function(e){var t=e[0],n=r();function r(e){return function(){var n=document.createElement("div");return t.render(_.apply(String,arguments),n,e),n.removeChild(n.firstChild)}}return n.options=r,n.block=r({displayMode:!0}),n})}}function z(){return c(function(e){var t=e(document.body.clientWidth);function n(){var n=document.body.clientWidth;n!==t&&e(t=n)}return window.addEventListener("resize",n),function(){window.removeEventListener("resize",n)}})}e.Library=function(e){const n=F(e);Object.defineProperties(this,{DOM:{value:i,writable:!0,enumerable:!0},Files:{value:u,writable:!0,enumerable:!0},Generators:{value:d,writable:!0,enumerable:!0},html:{value:t(m),writable:!0,enumerable:!0},md:{value:h(n),writable:!0,enumerable:!0},Mutable:{value:t(v),writable:!0,enumerable:!0},now:{value:w,writable:!0,enumerable:!0},Promises:{value:g,writable:!0,enumerable:!0},require:{value:t(n),writable:!0,enumerable:!0},resolve:{value:t(x),writable:!0,enumerable:!0},svg:{value:t(W),writable:!0,enumerable:!0},tex:{value:H(n),writable:!0,enumerable:!0},width:{value:z,writable:!0,enumerable:!0}})},Object.defineProperty(e,"__esModule",{value:!0})});