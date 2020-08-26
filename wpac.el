;;; wpac.el --- Wikipedia autocompletion -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.1
;; Keywords: convenience, autocompletion
;; URL: https://www.github.com/firmart/wpac
;; Package-Requires: ((emacs "26"))


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; wpac.el is a package providing autocompletion of Wikipedia articles, templates, etc.
;; Expected to use along with emacs-server.


;; Example:
;;
;; (wpac--results-count
;;  (wpac--get-response
;;   '(:action "query"
;;             :list "search"
;;             :format "json"
;;             :srnamespace 10
;;             :srsearch "Wikipedia"
;;             :srinfo "totalhits")))

;; TODO: POST with json instead of url parameters.

;; third-party
(require 'auto-complete)

;; built-in Emacs lib
(require 'json)
(require 'url-http)

;;; Code:
;;; Custom group
(defcustom wpac-base-url "https://en.wikipedia.org"
  "Wikimedia project url on which completion is done."
  :type  'string
  :group 'wpac
  :package-version '(wpac . "0.1"))

;;; Internal variables
(defvar wpac--api-path "/w/api.php?")
(defvar url-http-end-of-headers nil)
(defvar wpac--wikimedia-projects-list '("aa.wikibooks.org" "aa.wikipedia.org" "aa.wiktionary.org" "ab.wikipedia.org" "ab.wiktionary.org" "ace.wikipedia.org" "ady.wikipedia.org" "af.wikibooks.org" "af.wikipedia.org" "af.wikiquote.org" "af.wiktionary.org" "ak.wikibooks.org" "ak.wikipedia.org" "ak.wiktionary.org" "als.wikibooks.org" "als.wikipedia.org" "als.wikiquote.org" "als.wiktionary.org" "am.wikipedia.org" "am.wikiquote.org" "am.wiktionary.org" "an.wikipedia.org" "an.wiktionary.org" "ang.wikibooks.org" "ang.wikipedia.org" "ang.wikiquote.org" "ang.wikisource.org" "ang.wiktionary.org" "ar.wikibooks.org" "ar.wikimedia.org" "ar.wikinews.org" "ar.wikipedia.org" "ar.wikiquote.org" "ar.wikisource.org" "ar.wikiversity.org" "ar.wiktionary.org" "arc.wikipedia.org" "ary.wikipedia.org" "arz.wikipedia.org" "as.wikibooks.org" "as.wikipedia.org" "as.wikisource.org" "as.wiktionary.org" "ast.wikibooks.org" "ast.wikipedia.org" "ast.wikiquote.org" "ast.wiktionary.org" "atj.wikipedia.org" "av.wikipedia.org" "av.wiktionary.org" "avk.wikipedia.org" "awa.wikipedia.org" "ay.wikibooks.org" "ay.wikipedia.org" "ay.wiktionary.org" "az.wikibooks.org" "az.wikipedia.org" "az.wikiquote.org" "az.wikisource.org" "az.wiktionary.org" "azb.wikipedia.org" "ba.wikibooks.org" "ba.wikipedia.org" "ban.wikipedia.org" "bar.wikipedia.org" "bat-smg.wikipedia.org" "bcl.wikipedia.org" "be-tarask.wikipedia.org" "be.wikibooks.org" "be.wikipedia.org" "be.wikiquote.org" "be.wikisource.org" "be.wiktionary.org" "beta.wikiversity.org" "bg.wikibooks.org" "bg.wikinews.org" "bg.wikipedia.org" "bg.wikiquote.org" "bg.wikisource.org" "bg.wiktionary.org" "bh.wikipedia.org" "bh.wiktionary.org" "bi.wikibooks.org" "bi.wikipedia.org" "bi.wiktionary.org" "bjn.wikipedia.org" "bm.wikibooks.org" "bm.wikipedia.org" "bm.wikiquote.org" "bm.wiktionary.org" "bn.wikibooks.org" "bn.wikipedia.org" "bn.wikisource.org" "bn.wiktionary.org" "bo.wikibooks.org" "bo.wikipedia.org" "bo.wiktionary.org" "bpy.wikipedia.org" "br.wikimedia.org" "br.wikipedia.org" "br.wikiquote.org" "br.wikisource.org" "br.wiktionary.org" "bs.wikibooks.org" "bs.wikinews.org" "bs.wikipedia.org" "bs.wikiquote.org" "bs.wikisource.org" "bs.wiktionary.org" "bug.wikipedia.org" "bxr.wikipedia.org" "ca.wikibooks.org" "ca.wikinews.org" "ca.wikipedia.org" "ca.wikiquote.org" "ca.wikisource.org" "ca.wiktionary.org" "cbk-zam.wikipedia.org" "cdo.wikipedia.org" "ce.wikipedia.org" "ceb.wikipedia.org" "ch.wikibooks.org" "ch.wikipedia.org" "ch.wiktionary.org" "cho.wikipedia.org" "chr.wikipedia.org" "chr.wiktionary.org" "chy.wikipedia.org" "ckb.wikipedia.org" "co.wikibooks.org" "co.wikimedia.org" "co.wikipedia.org" "co.wikiquote.org" "co.wiktionary.org" "commons.wikimedia.org" "cr.wikipedia.org" "cr.wikiquote.org" "cr.wiktionary.org" "crh.wikipedia.org" "cs.wikibooks.org" "cs.wikinews.org" "cs.wikipedia.org" "cs.wikiquote.org" "cs.wikisource.org" "cs.wikiversity.org" "cs.wiktionary.org" "csb.wikipedia.org" "csb.wiktionary.org" "cu.wikipedia.org" "cv.wikibooks.org" "cv.wikipedia.org" "cy.wikibooks.org" "cy.wikipedia.org" "cy.wikiquote.org" "cy.wikisource.org" "cy.wiktionary.org" "da.wikibooks.org" "da.wikipedia.org" "da.wikiquote.org" "da.wikisource.org" "da.wiktionary.org" "de.wikibooks.org" "de.wikinews.org" "de.wikipedia.org" "de.wikiquote.org" "de.wikisource.org" "de.wikiversity.org" "de.wiktionary.org" "din.wikipedia.org" "diq.wikipedia.org" "dk.wikimedia.org" "dsb.wikipedia.org" "dty.wikipedia.org" "dv.wikipedia.org" "dv.wiktionary.org" "dz.wikipedia.org" "dz.wiktionary.org" "ee.wikipedia.org" "el.wikibooks.org" "el.wikinews.org" "el.wikipedia.org" "el.wikiquote.org" "el.wikisource.org" "el.wikiversity.org" "el.wiktionary.org" "eml.wikipedia.org" "en.wikibooks.org" "en.wikinews.org" "en.wikipedia.org" "en.wikiquote.org" "en.wikisource.org" "en.wikiversity.org" "en.wiktionary.org" "eo.wikibooks.org" "eo.wikinews.org" "eo.wikipedia.org" "eo.wikiquote.org" "eo.wikisource.org" "eo.wiktionary.org" "es.wikibooks.org" "es.wikinews.org" "es.wikipedia.org" "es.wikiquote.org" "es.wikisource.org" "es.wikiversity.org" "es.wiktionary.org" "et.wikibooks.org" "et.wikimedia.org" "et.wikipedia.org" "et.wikiquote.org" "et.wikisource.org" "et.wiktionary.org" "eu.wikibooks.org" "eu.wikipedia.org" "eu.wikiquote.org" "eu.wikisource.org" "eu.wiktionary.org" "ext.wikipedia.org" "fa.wikibooks.org" "fa.wikinews.org" "fa.wikipedia.org" "fa.wikiquote.org" "fa.wikisource.org" "fa.wiktionary.org" "ff.wikipedia.org" "fi.wikibooks.org" "fi.wikimedia.org" "fi.wikinews.org" "fi.wikipedia.org" "fi.wikiquote.org" "fi.wikisource.org" "fi.wikiversity.org" "fi.wiktionary.org" "fiu-vro.wikipedia.org" "fj.wikipedia.org" "fj.wiktionary.org" "fo.wikipedia.org" "fo.wikisource.org" "fo.wiktionary.org" "fr.wikibooks.org" "fr.wikinews.org" "fr.wikipedia.org" "fr.wikiquote.org" "fr.wikisource.org" "fr.wikiversity.org" "fr.wiktionary.org" "frp.wikipedia.org" "frr.wikipedia.org" "fur.wikipedia.org" "fy.wikibooks.org" "fy.wikipedia.org" "fy.wiktionary.org" "ga.wikibooks.org" "ga.wikipedia.org" "ga.wikiquote.org" "ga.wiktionary.org" "gag.wikipedia.org" "gan.wikipedia.org" "gcr.wikipedia.org" "gd.wikipedia.org" "gd.wiktionary.org" "ge.wikimedia.org" "gl.wikibooks.org" "gl.wikipedia.org" "gl.wikiquote.org" "gl.wikisource.org" "gl.wiktionary.org" "glk.wikipedia.org" "gn.wikibooks.org" "gn.wikipedia.org" "gn.wiktionary.org" "gom.wikipedia.org" "gom.wiktionary.org" "gor.wikipedia.org" "got.wikibooks.org" "got.wikipedia.org" "gr.wikimedia.org" "gu.wikibooks.org" "gu.wikipedia.org" "gu.wikiquote.org" "gu.wikisource.org" "gu.wiktionary.org" "gv.wikipedia.org" "gv.wiktionary.org" "ha.wikipedia.org" "ha.wiktionary.org" "hak.wikipedia.org" "haw.wikipedia.org" "he.wikibooks.org" "he.wikinews.org" "he.wikipedia.org" "he.wikiquote.org" "he.wikisource.org" "he.wiktionary.org" "hi.wikibooks.org" "hi.wikipedia.org" "hi.wikiquote.org" "hi.wikisource.org" "hi.wikiversity.org" "hi.wiktionary.org" "hif.wikipedia.org" "hif.wiktionary.org" "ho.wikipedia.org" "hr.wikibooks.org" "hr.wikipedia.org" "hr.wikiquote.org" "hr.wikisource.org" "hr.wiktionary.org" "hsb.wikipedia.org" "hsb.wiktionary.org" "ht.wikipedia.org" "ht.wikisource.org" "hu.wikibooks.org" "hu.wikinews.org" "hu.wikipedia.org" "hu.wikiquote.org" "hu.wikisource.org" "hu.wiktionary.org" "hy.wikibooks.org" "hy.wikipedia.org" "hy.wikiquote.org" "hy.wikisource.org" "hy.wiktionary.org" "hyw.wikipedia.org" "hz.wikipedia.org" "ia.wikibooks.org" "ia.wikipedia.org" "ia.wiktionary.org" "id.wikibooks.org" "id.wikipedia.org" "id.wikiquote.org" "id.wikisource.org" "id.wiktionary.org" "ie.wikibooks.org" "ie.wikipedia.org" "ie.wiktionary.org" "ig.wikipedia.org" "ii.wikipedia.org" "ik.wikipedia.org" "ik.wiktionary.org" "ilo.wikipedia.org" "incubator.wikimedia.org" "inh.wikipedia.org" "io.wikipedia.org" "io.wiktionary.org" "is.wikibooks.org" "is.wikipedia.org" "is.wikiquote.org" "is.wikisource.org" "is.wiktionary.org" "it.wikibooks.org" "it.wikinews.org" "it.wikipedia.org" "it.wikiquote.org" "it.wikisource.org" "it.wikiversity.org" "it.wiktionary.org" "iu.wikipedia.org" "iu.wiktionary.org" "ja.wikibooks.org" "ja.wikinews.org" "ja.wikipedia.org" "ja.wikiquote.org" "ja.wikisource.org" "ja.wikiversity.org" "ja.wiktionary.org" "jam.wikipedia.org" "jbo.wikipedia.org" "jbo.wiktionary.org" "jv.wikipedia.org" "jv.wiktionary.org" "ka.wikibooks.org" "ka.wikipedia.org" "ka.wikiquote.org" "ka.wiktionary.org" "kaa.wikipedia.org" "kab.wikipedia.org" "kbd.wikipedia.org" "kbp.wikipedia.org" "kg.wikipedia.org" "ki.wikipedia.org" "kj.wikipedia.org" "kk.wikibooks.org" "kk.wikipedia.org" "kk.wikiquote.org" "kk.wiktionary.org" "kl.wikipedia.org" "kl.wiktionary.org" "km.wikibooks.org" "km.wikipedia.org" "km.wiktionary.org" "kn.wikibooks.org" "kn.wikipedia.org" "kn.wikiquote.org" "kn.wikisource.org" "kn.wiktionary.org" "ko.wikibooks.org" "ko.wikinews.org" "ko.wikipedia.org" "ko.wikiquote.org" "ko.wikisource.org" "ko.wikiversity.org" "ko.wiktionary.org" "koi.wikipedia.org" "kr.wikipedia.org" "kr.wikiquote.org" "krc.wikipedia.org" "ks.wikibooks.org" "ks.wikipedia.org" "ks.wikiquote.org" "ks.wiktionary.org" "ksh.wikipedia.org" "ku.wikibooks.org" "ku.wikipedia.org" "ku.wikiquote.org" "ku.wiktionary.org" "kv.wikipedia.org" "kw.wikipedia.org" "kw.wikiquote.org" "kw.wiktionary.org" "ky.wikibooks.org" "ky.wikipedia.org" "ky.wikiquote.org" "ky.wiktionary.org" "la.wikibooks.org" "la.wikipedia.org" "la.wikiquote.org" "la.wikisource.org" "la.wiktionary.org" "lad.wikipedia.org" "lb.wikibooks.org" "lb.wikipedia.org" "lb.wikiquote.org" "lb.wiktionary.org" "lbe.wikipedia.org" "lez.wikipedia.org" "lfn.wikipedia.org" "lg.wikipedia.org" "li.wikibooks.org" "li.wikinews.org" "li.wikipedia.org" "li.wikiquote.org" "li.wikisource.org" "li.wiktionary.org" "lij.wikipedia.org" "lij.wikisource.org" "lld.wikipedia.org" "lmo.wikipedia.org" "ln.wikibooks.org" "ln.wikipedia.org" "ln.wiktionary.org" "lo.wikipedia.org" "lo.wiktionary.org" "lrc.wikipedia.org" "lt.wikibooks.org" "lt.wikipedia.org" "lt.wikiquote.org" "lt.wikisource.org" "lt.wiktionary.org" "ltg.wikipedia.org" "lv.wikibooks.org" "lv.wikipedia.org" "lv.wiktionary.org" "mai.wikipedia.org" "map-bms.wikipedia.org" "mdf.wikipedia.org" "meta.wikimedia.org" "mg.wikibooks.org" "mg.wikipedia.org" "mg.wiktionary.org" "mh.wikipedia.org" "mh.wiktionary.org" "mhr.wikipedia.org" "mi.wikibooks.org" "mi.wikipedia.org" "mi.wiktionary.org" "min.wikipedia.org" "min.wiktionary.org" "mk.wikibooks.org" "mk.wikimedia.org" "mk.wikipedia.org" "mk.wikisource.org" "mk.wiktionary.org" "ml.wikibooks.org" "ml.wikipedia.org" "ml.wikiquote.org" "ml.wikisource.org" "ml.wiktionary.org" "mn.wikibooks.org" "mn.wikipedia.org" "mn.wiktionary.org" "mnw.wikipedia.org" "mo.wiktionary.org" "mr.wikibooks.org" "mr.wikipedia.org" "mr.wikiquote.org" "mr.wikisource.org" "mr.wiktionary.org" "mrj.wikipedia.org" "ms.wikibooks.org" "ms.wikipedia.org" "ms.wiktionary.org" "mt.wikipedia.org" "mt.wiktionary.org" "mul.wikisource.org" "mus.wikipedia.org" "mwl.wikipedia.org" "mx.wikimedia.org" "my.wikibooks.org" "my.wikipedia.org" "my.wiktionary.org" "myv.wikipedia.org" "mzn.wikipedia.org" "na.wikibooks.org" "na.wikipedia.org" "na.wikiquote.org" "na.wiktionary.org" "nah.wikibooks.org" "nah.wikipedia.org" "nah.wiktionary.org" "nap.wikipedia.org" "nap.wikisource.org" "nds-nl.wikipedia.org" "nds.wikibooks.org" "nds.wikipedia.org" "nds.wikiquote.org" "nds.wiktionary.org" "ne.wikibooks.org" "ne.wikipedia.org" "ne.wiktionary.org" "new.wikipedia.org" "ng.wikimedia.org" "ng.wikipedia.org" "nl.wikibooks.org" "nl.wikimedia.org" "nl.wikinews.org" "nl.wikipedia.org" "nl.wikiquote.org" "nl.wikisource.org" "nl.wiktionary.org" "nn.wikipedia.org" "nn.wikiquote.org" "nn.wiktionary.org" "no.wikibooks.org" "no.wikimedia.org" "no.wikinews.org" "no.wikipedia.org" "no.wikiquote.org" "no.wikisource.org" "no.wiktionary.org" "nostalgia.wikipedia.org" "nov.wikipedia.org" "nqo.wikipedia.org" "nrm.wikipedia.org" "nso.wikipedia.org" "nv.wikipedia.org" "ny.wikipedia.org" "nz.wikimedia.org" "oc.wikibooks.org" "oc.wikipedia.org" "oc.wiktionary.org" "olo.wikipedia.org" "om.wikipedia.org" "om.wiktionary.org" "or.wikipedia.org" "or.wikisource.org" "or.wiktionary.org" "os.wikipedia.org" "outreach.wikimedia.org" "pa.us.wikimedia.org" "pa.wikibooks.org" "pa.wikipedia.org" "pa.wikisource.org" "pa.wiktionary.org" "pag.wikipedia.org" "pam.wikipedia.org" "pap.wikipedia.org" "pcd.wikipedia.org" "pdc.wikipedia.org" "pfl.wikipedia.org" "pi.wikipedia.org" "pi.wiktionary.org" "pih.wikipedia.org" "pl.wikibooks.org" "pl.wikimedia.org" "pl.wikinews.org" "pl.wikipedia.org" "pl.wikiquote.org" "pl.wikisource.org" "pl.wiktionary.org" "pms.wikipedia.org" "pms.wikisource.org" "pnb.wikipedia.org" "pnb.wiktionary.org" "pnt.wikipedia.org" "ps.wikibooks.org" "ps.wikipedia.org" "ps.wiktionary.org" "pt.wikibooks.org" "pt.wikimedia.org" "pt.wikimedia.org" "pt.wikinews.org" "pt.wikipedia.org" "pt.wikiquote.org" "pt.wikisource.org" "pt.wikiversity.org" "pt.wiktionary.org" "qu.wikibooks.org" "qu.wikipedia.org" "qu.wikiquote.org" "qu.wiktionary.org" "rm.wikibooks.org" "rm.wikipedia.org" "rm.wiktionary.org" "rmy.wikipedia.org" "rn.wikipedia.org" "rn.wiktionary.org" "ro.wikibooks.org" "ro.wikinews.org" "ro.wikipedia.org" "ro.wikiquote.org" "ro.wikisource.org" "ro.wiktionary.org" "roa-rup.wikipedia.org" "roa-rup.wiktionary.org" "roa-tara.wikipedia.org" "rs.wikimedia.org" "ru.wikibooks.org" "ru.wikimedia.org" "ru.wikinews.org" "ru.wikipedia.org" "ru.wikiquote.org" "ru.wikisource.org" "ru.wikiversity.org" "ru.wiktionary.org" "rue.wikipedia.org" "rw.wikipedia.org" "rw.wiktionary.org" "sa.wikibooks.org" "sa.wikipedia.org" "sa.wikiquote.org" "sa.wikisource.org" "sa.wiktionary.org" "sah.wikipedia.org" "sah.wikiquote.org" "sah.wikisource.org" "sat.wikipedia.org" "sc.wikipedia.org" "sc.wiktionary.org" "scn.wikipedia.org" "scn.wiktionary.org" "sco.wikipedia.org" "sd.wikinews.org" "sd.wikipedia.org" "sd.wiktionary.org" "se.wikibooks.org" "se.wikimedia.org" "se.wikipedia.org" "sg.wikipedia.org" "sg.wiktionary.org" "sh.wikipedia.org" "sh.wiktionary.org" "shn.wikipedia.org" "shn.wiktionary.org" "shy.wiktionary.org" "si.wikibooks.org" "si.wikipedia.org" "si.wiktionary.org" "simple.wikibooks.org" "simple.wikipedia.org" "simple.wikiquote.org" "simple.wiktionary.org" "sk.wikibooks.org" "sk.wikipedia.org" "sk.wikiquote.org" "sk.wikisource.org" "sk.wiktionary.org" "sl.wikibooks.org" "sl.wikipedia.org" "sl.wikiquote.org" "sl.wikisource.org" "sl.wikiversity.org" "sl.wiktionary.org" "sm.wikipedia.org" "sm.wiktionary.org" "sn.wikipedia.org" "sn.wiktionary.org" "so.wikipedia.org" "so.wiktionary.org" "species.wikimedia.org" "sq.wikibooks.org" "sq.wikinews.org" "sq.wikipedia.org" "sq.wikiquote.org" "sq.wiktionary.org" "sr.wikibooks.org" "sr.wikinews.org" "sr.wikipedia.org" "sr.wikiquote.org" "sr.wikisource.org" "sr.wiktionary.org" "srn.wikipedia.org" "ss.wikipedia.org" "ss.wiktionary.org" "st.wikipedia.org" "st.wiktionary.org" "stq.wikipedia.org" "strategy.wikimedia.org" "su.wikibooks.org" "su.wikipedia.org" "su.wikiquote.org" "su.wiktionary.org" "sv.wikibooks.org" "sv.wikinews.org" "sv.wikipedia.org" "sv.wikiquote.org" "sv.wikisource.org" "sv.wikiversity.org" "sv.wiktionary.org" "sw.wikibooks.org" "sw.wikipedia.org" "sw.wiktionary.org" "szl.wikipedia.org" "szy.wikipedia.org" "ta.wikibooks.org" "ta.wikinews.org" "ta.wikipedia.org" "ta.wikiquote.org" "ta.wikisource.org" "ta.wiktionary.org" "tcy.wikipedia.org" "te.wikibooks.org" "te.wikipedia.org" "te.wikiquote.org" "te.wikisource.org" "te.wiktionary.org" "ten.wikipedia.org" "test.wikipedia.org" "tet.wikipedia.org" "tg.wikibooks.org" "tg.wikipedia.org" "tg.wiktionary.org" "th.wikibooks.org" "th.wikinews.org" "th.wikipedia.org" "th.wikiquote.org" "th.wikisource.org" "th.wiktionary.org" "ti.wikipedia.org" "ti.wiktionary.org" "tk.wikibooks.org" "tk.wikipedia.org" "tk.wikiquote.org" "tk.wiktionary.org" "tl.wikibooks.org" "tl.wikipedia.org" "tl.wiktionary.org" "tn.wikipedia.org" "tn.wiktionary.org" "to.wikipedia.org" "to.wiktionary.org" "tpi.wikipedia.org" "tpi.wiktionary.org" "tr.wikibooks.org" "tr.wikimedia.org" "tr.wikinews.org" "tr.wikipedia.org" "tr.wikiquote.org" "tr.wikisource.org" "tr.wiktionary.org" "ts.wikipedia.org" "ts.wiktionary.org" "tt.wikibooks.org" "tt.wikipedia.org" "tt.wikiquote.org" "tt.wiktionary.org" "tum.wikipedia.org" "tw.wikipedia.org" "tw.wiktionary.org" "ty.wikipedia.org" "tyv.wikipedia.org" "ua.wikimedia.org" "udm.wikipedia.org" "ug.wikibooks.org" "ug.wikipedia.org" "ug.wikiquote.org" "ug.wiktionary.org" "uk.wikibooks.org" "uk.wikimedia.org" "uk.wikinews.org" "uk.wikipedia.org" "uk.wikiquote.org" "uk.wikisource.org" "uk.wiktionary.org" "ur.wikibooks.org" "ur.wikipedia.org" "ur.wikiquote.org" "ur.wiktionary.org" "usability.wikimedia.org" "uz.wikibooks.org" "uz.wikipedia.org" "uz.wikiquote.org" "uz.wiktionary.org" "ve.wikipedia.org" "vec.wikipedia.org" "vec.wikisource.org" "vec.wiktionary.org" "vep.wikipedia.org" "vi.wikibooks.org" "vi.wikipedia.org" "vi.wikiquote.org" "vi.wikisource.org" "vi.wiktionary.org" "vls.wikipedia.org" "vo.wikibooks.org" "vo.wikipedia.org" "vo.wikiquote.org" "vo.wiktionary.org" "wa.wikibooks.org" "wa.wikipedia.org" "wa.wiktionary.org" "war.wikipedia.org" "wikimania2005.wikimedia.org" "wikimania2006.wikimedia.org" "wikimania2007.wikimedia.org" "wikimania2008.wikimedia.org" "wikimania2009.wikimedia.org" "wikimania2010.wikimedia.org" "wikimania2011.wikimedia.org" "wikitech.wikimedia.org" "wo.wikipedia.org" "wo.wikiquote.org" "wo.wiktionary.org" "wuu.wikipedia.org" "www.mediawiki.org" "xal.wikipedia.org" "xh.wikibooks.org" "xh.wikipedia.org" "xh.wiktionary.org" "xmf.wikipedia.org" "yi.wikipedia.org" "yi.wikisource.org" "yi.wiktionary.org" "yo.wikibooks.org" "yo.wikipedia.org" "yo.wiktionary.org" "yue.wiktionary.org" "za.wikibooks.org" "za.wikipedia.org" "za.wikiquote.org" "za.wiktionary.org" "zea.wikipedia.org" "zh-classical.wikipedia.org" "zh-min-nan.wikibooks.org" "zh-min-nan.wikipedia.org" "zh-min-nan.wikiquote.org" "zh-min-nan.wikisource.org" "zh-min-nan.wiktionary.org" "zh-yue.wikipedia.org" "zh.wikibooks.org" "zh.wikinews.org" "zh.wikipedia.org" "zh.wikiquote.org" "zh.wikisource.org" "zh.wikiversity.org" "zh.wiktionary.org" "zu.wikibooks.org" "zu.wikipedia.org" "zu.wiktionary.org"))
;;; API
;;;; General
(defun wpac--form-url (plist)
  (concat 
   wpac-base-url
   wpac--api-path
   (wpac--plist-to-url-params plist)))

(defun wpac--get-response (plist &optional callback args)
  "Retrieve results from PLIST query, and apply CALLBACK with ARGS if succeed."
  (let* ((url (wpac--form-url plist))
         (buffer (url-retrieve-synchronously url))
         (code (url-http-symbol-value-in-buffer 'url-http-response-status buffer))
         (json-object-type 'plist)
         (results-count 0)
         (json))

    (when (= code 200)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^\r?\n" nil t)
        (backward-char 1)
        ;; Saw the end of the headers
        (setq url-http-end-of-headers (set-marker (make-marker) (point)))
        (setq json
              (json-read-from-string
               (buffer-substring url-http-end-of-headers (point-max)))))
      (unless (wpac--error-p json)
        (if callback
            (apply callback (list json args))
          json)))))

(defun wpac--error-p (plist)
  "Return error string if PLIST is an error, otherwise return nil."
  (when (plist-get plist :errors)
    (plist-get plist :*)))

(defmacro wpac--query-wrapper (method query)
  `(defun ,(intern (format "wpac-query-%s" method)) ,query
     ,(format "Retrieve results of `%s' with parameters `%s'." query (format "wpac--query-%s" method))
     (wpac--get-response
      ,@query
      #'wpac--plist-get-rec
      ,(intern (format "wpac--query-%s" method)))))

(defmacro wpac--interp-wrapper (method result)
  `(defun ,(intern (format "wpac-interp-%s" method)) ,result
     ,(format "Interpret results of `%s' with parameters `%s'." result (format "wpac--query-%s" method))
     (wpac--get-plist-get-rec
      ,@result
      ,(intern (format "wpac--query-%s" method)))))

;;;; Search
;; See https://www.mediawiki.org/wiki/API:Search
;;;;; Results count

(defvar wpac--query-results-count '(:query :searchinfo :totalhits))
(wpac--interp-wrapper "results-count" (result))
(wpac--query-wrapper "results-count" (query))

;;; Autocomplete
;;;; General
(defun wpac-config-default ()
  "Set default recommended configuration"
  (add-hook 'edit-server-start-hook 'wpac-edit-server-setup-buffer))
;;;; Prefix Search

(defvar wpac--query-prefix-search '(:query :prefixsearch))

(defun wpac--ac-prefix-template ()
  "Complete Wikipedia templates based on prefix search."
  (let ((query `(:action "query"
                         :list "prefixsearch"
                         :format "json"
                         :pslimit "500"
                         :psnamespace 10
                         :pssearch ,ac-prefix)))
    (message "%s" (wpac--form-url query))
    (mapcar
     ;; non-english projects return language specific equivalent prefix of "Template:"
     ;; such as "Modèle:" (fr), "模板:" (zh). We get rid of them.
     (lambda (e) (replace-regexp-in-string ".*?:\\(.*\\)" "\\1" (plist-get e :title)))
     (wpac--plist-get-rec
      (wpac--get-response query)
      wpac--query-prefix-search))))

;; TODO: replace wp-template prefix
;; (defun wpac--template-prefix-p ()
;;   "Existed (or to be existed) file prefix."
;;   (let* ((line-beg (line-beginning-position))
;;          (end (point))
;;          (start (or (let ((point (re-search-backward "[\"<>'= \t\r\n]" line-beg t)))
;;                      (if point (1+ point)))
;;                    line-beg))
;;          (file (buffer-substring start end)))
;;     (if (and file (or (string-match "^/" file)
;;                    (and (setq file (and (string-match "^[^/]*/" file)
;;                                     (match-string 0 file)))
;;                       (file-directory-p file))))
;;         (unless (ac-windows-remote-file-p file)
;;           start))))

(ac-define-source wp-template
  '((candidates . wpac--ac-prefix-template)
    (prefix . "{{\\(.*\\)")
    (requires . 0)
    (symbol . "t")
    (action . ac-start)))

;;;; Edit-server
(defun wpac--edit-server-base-url ()
  "Return the base url of the edit-server buffer"
  (require 'edit-server)
  (replace-regexp-in-string "\\(.*?\\)\\/.*" "\\1" edit-server-url))

(defun wpac--edit-server-valid-url-p ()
  "Return non-nil if the base url of the edit server buffer is valid."
  (member (wpac--edit-server-base-url) wpac--wikimedia-projects-list))

(defun wpac-edit-server-setup-buffer ()
  "Setup wikipedia auto-completion to be used in edit-server valid buffer."
  (interactive)
  (when (boundp 'edit-server-edit-mode)
    ;; in a wikimedia projects website
    (when (wpac--edit-server-valid-url-p)
      (make-local-variable 'wpac-base-url)
      (setq wpac-base-url (concat "https://" (wpac--edit-server-base-url "/"))
      (add-to-list 'ac-sources 'ac-source-wp-template)
      (auto-complete-mode t))))

;;; Misc.
;;;; plist

;; from https://emacs.stackexchange.com/a/10630/23697
(defun wpac--map-plist (fn plist)
  "Map PLIST values with function FN."
  (let ((pl    plist)
        (vals  ()))
    (while pl
      (push (funcall fn (car pl) (cadr pl)) vals)
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun wpac--plist-to-url-params (plist)
  "Convert a PLIST of form (:key . value) to a key1=value1&key2=value2 string."
  (string-join
   (mapcar
    (lambda (kv)
      (format "%s=%s"
              (substring (symbol-name (car kv)) 1)
              (cdr kv)))
    (wpac--map-plist #'cons plist))
   "&"))

(defun wpac--plist-get-rec (plist keys)
  "Recursively find KEYS in PLIST."
  (while keys
    (setq plist (plist-get plist (pop keys))))
  plist)

(provide 'wpac)


;;; wpac.el ends here
