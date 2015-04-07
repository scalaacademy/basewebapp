//  Copyright (c) 2014 David Miguel Antunes <davidmiguel {at} antunes.net>
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.

package com.github.david04.liftutils.fontawesome

import scala.xml.{NodeSeq, Text}


trait Icon {

  def clazz: String

  def shortName: String

  def name: String

  def nodeSeq: NodeSeq
}

/**
 * 3.2.1
 */
object Icon_3_2_1 {

  //cat /tmp/icon | cut -d " " -f 1|while read i ; do echo "val $(echo "$i"|sed 's/icon-//g'|sed 's/-/_/g') = Value(\"$i\")"; done|sort
  // cat /tmp/icon1 | while read i ; do echo "val $(echo $(echo $i |cut -d "\"" -f 2)|sed 's/icon-//g'|sed 's/-/_/g') = FAIcon(\"$(echo $i |cut -d "\"" -f 2)\", \"$(echo $i |cut -d "\"" -f 4)\")"; done|sort

  case class FAIcon(clazz: String, code: String) extends Icon {
    lazy val name =
      clazz.toString
        .replaceAll("^fa-", "")
        .replace("-", " ")
        .foldLeft("")((acc, c) => if (acc.endsWith(" ")) acc + c.toUpper else acc + c)
        .capitalize

    lazy val shortName = name.replace("Icon ", "")

    lazy val nodeSeq = Text(Integer.parseInt(code, 16).toChar + "")
  }

  val adjust = FAIcon("icon-adjust", "f042")
  val adn = FAIcon("icon-adn", "f170")
  val align_center = FAIcon("icon-align-center", "f037")
  val align_justify = FAIcon("icon-align-justify", "f039")
  val align_left = FAIcon("icon-align-left", "f036")
  val align_right = FAIcon("icon-align-right", "f038")
  val ambulance = FAIcon("icon-ambulance", "f0f9")
  val anchor = FAIcon("icon-anchor", "f13d")
  val android = FAIcon("icon-android", "f17b")
  val angle_down = FAIcon("icon-angle-down", "f107")
  val angle_left = FAIcon("icon-angle-left", "f104")
  val angle_right = FAIcon("icon-angle-right", "f105")
  val angle_up = FAIcon("icon-angle-up", "f106")
  val apple = FAIcon("icon-apple", "f179")
  val archive = FAIcon("icon-archive", "f187")
  val arrow_down = FAIcon("icon-arrow-down", "f063")
  val arrow_left = FAIcon("icon-arrow-left", "f060")
  val arrow_right = FAIcon("icon-arrow-right", "f061")
  val arrow_up = FAIcon("icon-arrow-up", "f062")
  val asterisk = FAIcon("icon-asterisk", "f069")
  val backward = FAIcon("icon-backward", "f04a")
  val ban_circle = FAIcon("icon-ban-circle", "f05e")
  val bar_chart = FAIcon("icon-bar-chart", "f080")
  val barcode = FAIcon("icon-barcode", "f02a")
  val beaker = FAIcon("icon-beaker", "f0c3")
  val beer = FAIcon("icon-beer", "f0fc")
  val bell_alt = FAIcon("icon-bell-alt", "f0f3")
  val bell = FAIcon("icon-bell", "f0a2")
  val bitbucket = FAIcon("icon-bitbucket", "f171")
  val bitbucket_sign = FAIcon("icon-bitbucket-sign", "f172")
  val bold = FAIcon("icon-bold", "f032")
  val bolt = FAIcon("icon-bolt", "f0e7")
  val book = FAIcon("icon-book", "f02d")
  val bookmark_empty = FAIcon("icon-bookmark-empty", "f097")
  val bookmark = FAIcon("icon-bookmark", "f02e")
  val briefcase = FAIcon("icon-briefcase", "f0b1")
  val btc = FAIcon("icon-btc", "f15a")
  val bug = FAIcon("icon-bug", "f188")
  val building = FAIcon("icon-building", "f0f7")
  val bullhorn = FAIcon("icon-bullhorn", "f0a1")
  val bullseye = FAIcon("icon-bullseye", "f140")
  val calendar_empty = FAIcon("icon-calendar-empty", "f133")
  val calendar = FAIcon("icon-calendar", "f073")
  val camera = FAIcon("icon-camera", "f030")
  val camera_retro = FAIcon("icon-camera-retro", "f083")
  val caret_down = FAIcon("icon-caret-down", "f0d7")
  val caret_left = FAIcon("icon-caret-left", "f0d9")
  val caret_right = FAIcon("icon-caret-right", "f0da")
  val caret_up = FAIcon("icon-caret-up", "f0d8")
  val certificate = FAIcon("icon-certificate", "f0a3")
  val check_empty = FAIcon("icon-check-empty", "f096")
  val check = FAIcon("icon-check", "f046")
  val check_minus = FAIcon("icon-check-minus", "f147")
  val check_sign = FAIcon("icon-check-sign", "f14a")
  val chevron_down = FAIcon("icon-chevron-down", "f078")
  val chevron_left = FAIcon("icon-chevron-left", "f053")
  val chevron_right = FAIcon("icon-chevron-right", "f054")
  val chevron_sign_down = FAIcon("icon-chevron-sign-down", "f13a")
  val chevron_sign_left = FAIcon("icon-chevron-sign-left", "f137")
  val chevron_sign_right = FAIcon("icon-chevron-sign-right", "f138")
  val chevron_sign_up = FAIcon("icon-chevron-sign-up", "f139")
  val chevron_up = FAIcon("icon-chevron-up", "f077")
  val circle_arrow_down = FAIcon("icon-circle-arrow-down", "f0ab")
  val circle_arrow_left = FAIcon("icon-circle-arrow-left", "f0a8")
  val circle_arrow_right = FAIcon("icon-circle-arrow-right", "f0a9")
  val circle_arrow_up = FAIcon("icon-circle-arrow-up", "f0aa")
  val circle_blank = FAIcon("icon-circle-blank", "f10c")
  val circle = FAIcon("icon-circle", "f111")
  val cloud_download = FAIcon("icon-cloud-download", "f0ed")
  val cloud = FAIcon("icon-cloud", "f0c2")
  val cloud_upload = FAIcon("icon-cloud-upload", "f0ee")
  val cny = FAIcon("icon-cny", "f158")
  val code = FAIcon("icon-code", "f121")
  val code_fork = FAIcon("icon-code-fork", "f126")
  val coffee = FAIcon("icon-coffee", "f0f4")
  val cog = FAIcon("icon-cog", "f013")
  val cogs = FAIcon("icon-cogs", "f085")
  val collapse_alt = FAIcon("icon-collapse-alt", "f117")
  val collapse = FAIcon("icon-collapse", "f150")
  val collapse_top = FAIcon("icon-collapse-top", "f151")
  val columns = FAIcon("icon-columns", "f0db")
  val comment_alt = FAIcon("icon-comment-alt", "f0e5")
  val comment = FAIcon("icon-comment", "f075")
  val comments_alt = FAIcon("icon-comments-alt", "f0e6")
  val comments = FAIcon("icon-comments", "f086")
  val compass = FAIcon("icon-compass", "f14e")
  val copy = FAIcon("icon-copy", "f0c5")
  val credit_card = FAIcon("icon-credit-card", "f09d")
  val crop = FAIcon("icon-crop", "f125")
  val css3 = FAIcon("icon-css3", "f13c")
  val cut = FAIcon("icon-cut", "f0c4")
  val dashboard = FAIcon("icon-dashboard", "f0e4")
  val desktop = FAIcon("icon-desktop", "f108")
  val double_angle_down = FAIcon("icon-double-angle-down", "f103")
  val double_angle_left = FAIcon("icon-double-angle-left", "f100")
  val double_angle_right = FAIcon("icon-double-angle-right", "f101")
  val double_angle_up = FAIcon("icon-double-angle-up", "f102")
  val download_alt = FAIcon("icon-download-alt", "f019")
  val download = FAIcon("icon-download", "f01a")
  val dribbble = FAIcon("icon-dribbble", "f17d")
  val dropbox = FAIcon("icon-dropbox", "f16b")
  val edit = FAIcon("icon-edit", "f044")
  val edit_sign = FAIcon("icon-edit-sign", "f14b")
  val eject = FAIcon("icon-eject", "f052")
  val ellipsis_horizontal = FAIcon("icon-ellipsis-horizontal", "f141")
  val ellipsis_vertical = FAIcon("icon-ellipsis-vertical", "f142")
  val envelope_alt = FAIcon("icon-envelope-alt", "f003")
  val envelope = FAIcon("icon-envelope", "f0e0")
  val eraser = FAIcon("icon-eraser", "f12d")
  val eur = FAIcon("icon-eur", "f153")
  val exchange = FAIcon("icon-exchange", "f0ec")
  val exclamation = FAIcon("icon-exclamation", "f12a")
  val exclamation_sign = FAIcon("icon-exclamation-sign", "f06a")
  val expand_alt = FAIcon("icon-expand-alt", "f116")
  val expand = FAIcon("icon-expand", "f152")
  val external_link = FAIcon("icon-external-link", "f08e")
  val external_link_sign = FAIcon("icon-external-link-sign", "f14c")
  val eye_close = FAIcon("icon-eye-close", "f070")
  val eye_open = FAIcon("icon-eye-open", "f06e")
  val facebook = FAIcon("icon-facebook", "f09a")
  val facebook_sign = FAIcon("icon-facebook-sign", "f082")
  val facetime_video = FAIcon("icon-facetime-video", "f03d")
  val fast_backward = FAIcon("icon-fast-backward", "f049")
  val fast_forward = FAIcon("icon-fast-forward", "f050")
  val female = FAIcon("icon-female", "f182")
  val fighter_jet = FAIcon("icon-fighter-jet", "f0fb")
  val file_alt = FAIcon("icon-file-alt", "f016")
  val file = FAIcon("icon-file", "f15b")
  val file_text_alt = FAIcon("icon-file-text-alt", "f0f6")
  val file_text = FAIcon("icon-file-text", "f15c")
  val film = FAIcon("icon-film", "f008")
  val filter = FAIcon("icon-filter", "f0b0")
  val fire_extinguisher = FAIcon("icon-fire-extinguisher", "f134")
  val fire = FAIcon("icon-fire", "f06d")
  val flag_alt = FAIcon("icon-flag-alt", "f11d")
  val flag_checkered = FAIcon("icon-flag-checkered", "f11e")
  val flag = FAIcon("icon-flag", "f024")
  val flickr = FAIcon("icon-flickr", "f16e")
  val folder_close_alt = FAIcon("icon-folder-close-alt", "f114")
  val folder_close = FAIcon("icon-folder-close", "f07b")
  val folder_open_alt = FAIcon("icon-folder-open-alt", "f115")
  val folder_open = FAIcon("icon-folder-open", "f07c")
  val font = FAIcon("icon-font", "f031")
  val food = FAIcon("icon-food", "f0f5")
  val forward = FAIcon("icon-forward", "f04e")
  val foursquare = FAIcon("icon-foursquare", "f180")
  val frown = FAIcon("icon-frown", "f119")
  val fullscreen = FAIcon("icon-fullscreen", "f0b2")
  val gamepad = FAIcon("icon-gamepad", "f11b")
  val gbp = FAIcon("icon-gbp", "f154")
  val gift = FAIcon("icon-gift", "f06b")
  val github_alt = FAIcon("icon-github-alt", "f113")
  val github = FAIcon("icon-github", "f09b")
  val github_sign = FAIcon("icon-github-sign", "f092")
  val gittip = FAIcon("icon-gittip", "f184")
  val glass = FAIcon("icon-glass", "f000")
  val globe = FAIcon("icon-globe", "f0ac")
  val google_plus = FAIcon("icon-google-plus", "f0d5")
  val google_plus_sign = FAIcon("icon-google-plus-sign", "f0d4")
  val group = FAIcon("icon-group", "f0c0")
  val hand_down = FAIcon("icon-hand-down", "f0a7")
  val hand_left = FAIcon("icon-hand-left", "f0a5")
  val hand_right = FAIcon("icon-hand-right", "f0a4")
  val hand_up = FAIcon("icon-hand-up", "f0a6")
  val hdd = FAIcon("icon-hdd", "f0a0")
  val headphones = FAIcon("icon-headphones", "f025")
  val heart_empty = FAIcon("icon-heart-empty", "f08a")
  val heart = FAIcon("icon-heart", "f004")
  val home = FAIcon("icon-home", "f015")
  val hospital = FAIcon("icon-hospital", "f0f8")
  val h_sign = FAIcon("icon-h-sign", "f0fd")
  val html5 = FAIcon("icon-html5", "f13b")
  val inbox = FAIcon("icon-inbox", "f01c")
  val indent_left = FAIcon("icon-indent-left", "f03b")
  val indent_right = FAIcon("icon-indent-right", "f03c")
  val info = FAIcon("icon-info", "f129")
  val info_sign = FAIcon("icon-info-sign", "f05a")
  val inr = FAIcon("icon-inr", "f156")
  val instagram = FAIcon("icon-instagram", "f16d")
  val italic = FAIcon("icon-italic", "f033")
  val jpy = FAIcon("icon-jpy", "f157")
  val keyboard = FAIcon("icon-keyboard", "f11c")
  val key = FAIcon("icon-key", "f084")
  val krw = FAIcon("icon-krw", "f159")
  val laptop = FAIcon("icon-laptop", "f109")
  val leaf = FAIcon("icon-leaf", "f06c")
  val legal = FAIcon("icon-legal", "f0e3")
  val lemon = FAIcon("icon-lemon", "f094")
  val level_down = FAIcon("icon-level-down", "f149")
  val level_up = FAIcon("icon-level-up", "f148")
  val lightbulb = FAIcon("icon-lightbulb", "f0eb")
  val linkedin = FAIcon("icon-linkedin", "f0e1")
  val linkedin_sign = FAIcon("icon-linkedin-sign", "f08c")
  val link = FAIcon("icon-link", "f0c1")
  val linux = FAIcon("icon-linux", "f17c")
  val list_alt = FAIcon("icon-list-alt", "f022")
  val list = FAIcon("icon-list", "f03a")
  val list_ol = FAIcon("icon-list-ol", "f0cb")
  val list_ul = FAIcon("icon-list-ul", "f0ca")
  val location_arrow = FAIcon("icon-location-arrow", "f124")
  val lock = FAIcon("icon-lock", "f023")
  val long_arrow_down = FAIcon("icon-long-arrow-down", "f175")
  val long_arrow_left = FAIcon("icon-long-arrow-left", "f177")
  val long_arrow_right = FAIcon("icon-long-arrow-right", "f178")
  val long_arrow_up = FAIcon("icon-long-arrow-up", "f176")
  val magic = FAIcon("icon-magic", "f0d0")
  val magnet = FAIcon("icon-magnet", "f076")
  val mail_reply_all = FAIcon("icon-mail-reply-all", "f122")
  val male = FAIcon("icon-male", "f183")
  val map_marker = FAIcon("icon-map-marker", "f041")
  val maxcdn = FAIcon("icon-maxcdn", "f136")
  val medkit = FAIcon("icon-medkit", "f0fa")
  val meh = FAIcon("icon-meh", "f11a")
  val microphone = FAIcon("icon-microphone", "f130")
  val microphone_off = FAIcon("icon-microphone-off", "f131")
  val minus = FAIcon("icon-minus", "f068")
  val minus_sign_alt = FAIcon("icon-minus-sign-alt", "f146")
  val minus_sign = FAIcon("icon-minus-sign", "f056")
  val mobile_phone = FAIcon("icon-mobile-phone", "f10b")
  val money = FAIcon("icon-money", "f0d6")
  val moon = FAIcon("icon-moon", "f186")
  val move = FAIcon("icon-move", "f047")
  val music = FAIcon("icon-music", "f001")
  val off = FAIcon("icon-off", "f011")
  val ok_circle = FAIcon("icon-ok-circle", "f05d")
  val ok = FAIcon("icon-ok", "f00c")
  val ok_sign = FAIcon("icon-ok-sign", "f058")
  val paper_clip = FAIcon("icon-paper-clip", "f0c6")
  val paste = FAIcon("icon-paste", "f0ea")
  val pause = FAIcon("icon-pause", "f04c")
  val pencil = FAIcon("icon-pencil", "f040")
  val phone = FAIcon("icon-phone", "f095")
  val phone_sign = FAIcon("icon-phone-sign", "f098")
  val picture = FAIcon("icon-picture", "f03e")
  val pinterest = FAIcon("icon-pinterest", "f0d2")
  val pinterest_sign = FAIcon("icon-pinterest-sign", "f0d3")
  val plane = FAIcon("icon-plane", "f072")
  val play_circle = FAIcon("icon-play-circle", "f01d")
  val play = FAIcon("icon-play", "f04b")
  val play_sign = FAIcon("icon-play-sign", "f144")
  val plus = FAIcon("icon-plus", "f067")
  val plus_sign_alt = FAIcon("icon-plus-sign-alt", "f0fe")
  val plus_sign = FAIcon("icon-plus-sign", "f055")
  val print = FAIcon("icon-print", "f02f")
  val pushpin = FAIcon("icon-pushpin", "f08d")
  val puzzle_piece = FAIcon("icon-puzzle-piece", "f12e")
  val qrcode = FAIcon("icon-qrcode", "f029")
  val question = FAIcon("icon-question", "f128")
  val question_sign = FAIcon("icon-question-sign", "f059")
  val quote_left = FAIcon("icon-quote-left", "f10d")
  val quote_right = FAIcon("icon-quote-right", "f10e")
  val random = FAIcon("icon-random", "f074")
  val refresh = FAIcon("icon-refresh", "f021")
  val remove_circle = FAIcon("icon-remove-circle", "f05c")
  val remove = FAIcon("icon-remove", "f00d")
  val remove_sign = FAIcon("icon-remove-sign", "f057")
  val renren = FAIcon("icon-renren", "f18b")
  val reorder = FAIcon("icon-reorder", "f0c9")
  val repeat = FAIcon("icon-repeat", "f01e")
  val reply_all = FAIcon("icon-reply-all", "f122")
  val reply = FAIcon("icon-reply", "f112")
  val resize_full = FAIcon("icon-resize-full", "f065")
  val resize_horizontal = FAIcon("icon-resize-horizontal", "f07e")
  val resize_small = FAIcon("icon-resize-small", "f066")
  val resize_vertical = FAIcon("icon-resize-vertical", "f07d")
  val retweet = FAIcon("icon-retweet", "f079")
  val road = FAIcon("icon-road", "f018")
  val rocket = FAIcon("icon-rocket", "f135")
  val rss = FAIcon("icon-rss", "f09e")
  val rss_sign = FAIcon("icon-rss-sign", "f143")
  val save = FAIcon("icon-save", "f0c7")
  val screenshot = FAIcon("icon-screenshot", "f05b")
  val search = FAIcon("icon-search", "f002")
  val share_alt = FAIcon("icon-share-alt", "f064")
  val share = FAIcon("icon-share", "f045")
  val share_sign = FAIcon("icon-share-sign", "f14d")
  val shield = FAIcon("icon-shield", "f132")
  val shopping_cart = FAIcon("icon-shopping-cart", "f07a")
  val signal = FAIcon("icon-signal", "f012")
  val sign_blank = FAIcon("icon-sign-blank", "f0c8")
  val signin = FAIcon("icon-signin", "f090")
  val signout = FAIcon("icon-signout", "f08b")
  val sitemap = FAIcon("icon-sitemap", "f0e8")
  val skype = FAIcon("icon-skype", "f17e")
  val smile = FAIcon("icon-smile", "f118")
  val sort_by_alphabet_alt = FAIcon("icon-sort-by-alphabet-alt", "f15e")
  val sort_by_alphabet = FAIcon("icon-sort-by-alphabet", "f15d")
  val sort_by_attributes_alt = FAIcon("icon-sort-by-attributes-alt", "f161")
  val sort_by_attributes = FAIcon("icon-sort-by-attributes", "f160")
  val sort_by_order_alt = FAIcon("icon-sort-by-order-alt", "f163")
  val sort_by_order = FAIcon("icon-sort-by-order", "f162")
  val sort_down = FAIcon("icon-sort-down", "f0dd")
  val sort = FAIcon("icon-sort", "f0dc")
  val sort_up = FAIcon("icon-sort-up", "f0de")
  val spinner = FAIcon("icon-spinner", "f110")
  val stackexchange = FAIcon("icon-stackexchange", "f16c")
  val star_empty = FAIcon("icon-star-empty", "f006")
  val star = FAIcon("icon-star", "f005")
  val star_half_empty = FAIcon("icon-star-half-empty", "f123")
  val star_half = FAIcon("icon-star-half", "f089")
  val step_backward = FAIcon("icon-step-backward", "f048")
  val step_forward = FAIcon("icon-step-forward", "f051")
  val stethoscope = FAIcon("icon-stethoscope", "f0f1")
  val stop = FAIcon("icon-stop", "f04d")
  val strikethrough = FAIcon("icon-strikethrough", "f0cc")
  val subscript = FAIcon("icon-subscript", "f12c")
  val suitcase = FAIcon("icon-suitcase", "f0f2")
  val sun = FAIcon("icon-sun", "f185")
  val superscript = FAIcon("icon-superscript", "f12b")
  val table = FAIcon("icon-table", "f0ce")
  val tablet = FAIcon("icon-tablet", "f10a")
  val tag = FAIcon("icon-tag", "f02b")
  val tags = FAIcon("icon-tags", "f02c")
  val tasks = FAIcon("icon-tasks", "f0ae")
  val terminal = FAIcon("icon-terminal", "f120")
  val text_height = FAIcon("icon-text-height", "f034")
  val text_width = FAIcon("icon-text-width", "f035")
  val th = FAIcon("icon-th", "f00a")
  val th_large = FAIcon("icon-th-large", "f009")
  val th_list = FAIcon("icon-th-list", "f00b")
  val thumbs_down_alt = FAIcon("icon-thumbs-down-alt", "f088")
  val thumbs_down = FAIcon("icon-thumbs-down", "f165")
  val thumbs_up_alt = FAIcon("icon-thumbs-up-alt", "f087")
  val thumbs_up = FAIcon("icon-thumbs-up", "f164")
  val ticket = FAIcon("icon-ticket", "f145")
  val time = FAIcon("icon-time", "f017")
  val tint = FAIcon("icon-tint", "f043")
  val trash = FAIcon("icon-trash", "f014")
  val trello = FAIcon("icon-trello", "f181")
  val trophy = FAIcon("icon-trophy", "f091")
  val truck = FAIcon("icon-truck", "f0d1")
  val tumblr = FAIcon("icon-tumblr", "f173")
  val tumblr_sign = FAIcon("icon-tumblr-sign", "f174")
  val twitter = FAIcon("icon-twitter", "f099")
  val twitter_sign = FAIcon("icon-twitter-sign", "f081")
  val umbrella = FAIcon("icon-umbrella", "f0e9")
  val underline = FAIcon("icon-underline", "f0cd")
  val undo = FAIcon("icon-undo", "f0e2")
  val unlink = FAIcon("icon-unlink", "f127")
  val unlock_alt = FAIcon("icon-unlock-alt", "f13e")
  val unlock = FAIcon("icon-unlock", "f09c")
  val upload_alt = FAIcon("icon-upload-alt", "f093")
  val upload = FAIcon("icon-upload", "f01b")
  val usd = FAIcon("icon-usd", "f155")
  val user = FAIcon("icon-user", "f007")
  val user_md = FAIcon("icon-user-md", "f0f0")
  val vk = FAIcon("icon-vk", "f189")
  val volume_down = FAIcon("icon-volume-down", "f027")
  val volume_off = FAIcon("icon-volume-off", "f026")
  val volume_up = FAIcon("icon-volume-up", "f028")
  val warning_sign = FAIcon("icon-warning-sign", "f071")
  val weibo = FAIcon("icon-weibo", "f18a")
  val windows = FAIcon("icon-windows", "f17a")
  val wrench = FAIcon("icon-wrench", "f0ad")
  val xing = FAIcon("icon-xing", "f168")
  val xing_sign = FAIcon("icon-xing-sign", "f169")
  val youtube = FAIcon("icon-youtube", "f167")
  val youtube_play = FAIcon("icon-youtube-play", "f16a")
  val youtube_sign = FAIcon("icon-youtube-sign", "f166")
  val zoom_in = FAIcon("icon-zoom-in", "f00e")
  val zoom_out = FAIcon("icon-zoom-out", "f010")

  lazy val byClass = all.map(i => (i.clazz, i)).toMap

  lazy val all = Seq(
    adjust,
    adn,
    align_center,
    align_justify,
    align_left,
    align_right,
    ambulance,
    anchor,
    android,
    angle_down,
    angle_left,
    angle_right,
    angle_up,
    apple,
    archive,
    arrow_down,
    arrow_left,
    arrow_right,
    arrow_up,
    asterisk,
    backward,
    ban_circle,
    bar_chart,
    barcode,
    beaker,
    beer,
    bell_alt,
    bell,
    bitbucket,
    bitbucket_sign,
    bold,
    bolt,
    book,
    bookmark_empty,
    bookmark,
    briefcase,
    btc,
    bug,
    building,
    bullhorn,
    bullseye,
    calendar_empty,
    calendar,
    camera,
    camera_retro,
    caret_down,
    caret_left,
    caret_right,
    caret_up,
    certificate,
    check_empty,
    check,
    check_minus,
    check_sign,
    chevron_down,
    chevron_left,
    chevron_right,
    chevron_sign_down,
    chevron_sign_left,
    chevron_sign_right,
    chevron_sign_up,
    chevron_up,
    circle_arrow_down,
    circle_arrow_left,
    circle_arrow_right,
    circle_arrow_up,
    circle_blank,
    circle,
    cloud_download,
    cloud,
    cloud_upload,
    cny,
    code,
    code_fork,
    coffee,
    cog,
    cogs,
    collapse_alt,
    collapse,
    collapse_top,
    columns,
    comment_alt,
    comment,
    comments_alt,
    comments,
    compass,
    copy,
    credit_card,
    crop,
    css3,
    cut,
    dashboard,
    desktop,
    double_angle_down,
    double_angle_left,
    double_angle_right,
    double_angle_up,
    download_alt,
    download,
    dribbble,
    dropbox,
    edit,
    edit_sign,
    eject,
    ellipsis_horizontal,
    ellipsis_vertical,
    envelope_alt,
    envelope,
    eraser,
    eur,
    exchange,
    exclamation,
    exclamation_sign,
    expand_alt,
    expand,
    external_link,
    external_link_sign,
    eye_close,
    eye_open,
    facebook,
    facebook_sign,
    facetime_video,
    fast_backward,
    fast_forward,
    female,
    fighter_jet,
    file_alt,
    file,
    file_text_alt,
    file_text,
    film,
    filter,
    fire_extinguisher,
    fire,
    flag_alt,
    flag_checkered,
    flag,
    flickr,
    folder_close_alt,
    folder_close,
    folder_open_alt,
    folder_open,
    font,
    food,
    forward,
    foursquare,
    frown,
    fullscreen,
    gamepad,
    gbp,
    gift,
    github_alt,
    github,
    github_sign,
    gittip,
    glass,
    globe,
    google_plus,
    google_plus_sign,
    group,
    hand_down,
    hand_left,
    hand_right,
    hand_up,
    hdd,
    headphones,
    heart_empty,
    heart,
    home,
    hospital,
    h_sign,
    html5,
    inbox,
    indent_left,
    indent_right,
    info,
    info_sign,
    inr,
    instagram,
    italic,
    jpy,
    keyboard,
    key,
    krw,
    laptop,
    leaf,
    legal,
    lemon,
    level_down,
    level_up,
    lightbulb,
    linkedin,
    linkedin_sign,
    link,
    linux,
    list_alt,
    list,
    list_ol,
    list_ul,
    location_arrow,
    lock,
    long_arrow_down,
    long_arrow_left,
    long_arrow_right,
    long_arrow_up,
    magic,
    magnet,
    mail_reply_all,
    male,
    map_marker,
    maxcdn,
    medkit,
    meh,
    microphone,
    microphone_off,
    minus,
    minus_sign_alt,
    minus_sign,
    mobile_phone,
    money,
    moon,
    move,
    music,
    off,
    ok_circle,
    ok,
    ok_sign,
    paper_clip,
    paste,
    pause,
    pencil,
    phone,
    phone_sign,
    picture,
    pinterest,
    pinterest_sign,
    plane,
    play_circle,
    play,
    play_sign,
    plus,
    plus_sign_alt,
    plus_sign,
    print,
    pushpin,
    puzzle_piece,
    qrcode,
    question,
    question_sign,
    quote_left,
    quote_right,
    random,
    refresh,
    remove_circle,
    remove,
    remove_sign,
    renren,
    reorder,
    repeat,
    reply_all,
    reply,
    resize_full,
    resize_horizontal,
    resize_small,
    resize_vertical,
    retweet,
    road,
    rocket,
    rss,
    rss_sign,
    save,
    screenshot,
    search,
    share_alt,
    share,
    share_sign,
    shield,
    shopping_cart,
    signal,
    sign_blank,
    signin,
    signout,
    sitemap,
    skype,
    smile,
    sort_by_alphabet_alt,
    sort_by_alphabet,
    sort_by_attributes_alt,
    sort_by_attributes,
    sort_by_order_alt,
    sort_by_order,
    sort_down,
    sort,
    sort_up,
    spinner,
    stackexchange,
    star_empty,
    star,
    star_half_empty,
    star_half,
    step_backward,
    step_forward,
    stethoscope,
    stop,
    strikethrough,
    subscript,
    suitcase,
    sun,
    superscript,
    table,
    tablet,
    tag,
    tags,
    tasks,
    terminal,
    text_height,
    text_width,
    th,
    th_large,
    th_list,
    thumbs_down_alt,
    thumbs_down,
    thumbs_up_alt,
    thumbs_up,
    ticket,
    time,
    tint,
    trash,
    trello,
    trophy,
    truck,
    tumblr,
    tumblr_sign,
    twitter,
    twitter_sign,
    umbrella,
    underline,
    undo,
    unlink,
    unlock_alt,
    unlock,
    upload_alt,
    upload,
    usd,
    user,
    user_md,
    vk,
    volume_down,
    volume_off,
    volume_up,
    warning_sign,
    weibo,
    windows,
    wrench,
    xing,
    xing_sign,
    youtube,
    youtube_play,
    youtube_sign,
    zoom_in,
    zoom_out
  )
}
