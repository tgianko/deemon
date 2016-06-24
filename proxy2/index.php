<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" dir="ltr" lang="en" xml:lang="en">
<head><title>Your Store</title>
<meta charset="UTF-8">
<!--[if IE]>
	<meta http-equiv="x-ua-compatible" content="IE=Edge" />
<![endif]-->
<meta name="keywords" content="keyword1,keyword2,keyword3"/>
<meta name="description" content="Web Store Meta Description"/>
<meta name="generator" content="AbanteCart v1.2.4 - Open Source eCommerce solution"/>

<meta name="viewport" content="width=device-width, initial-scale=1.0"/>

<base href="http://192.168.56.102/"/>

<link href="resources/image/18/73/4.ico" type="image/png" rel="icon"/>

<link href="storefront/view/default/image/apple-touch-icon.png" rel="apple-touch-icon"/>
<link href="storefront/view/default/image/apple-touch-icon-76x76.png" rel="apple-touch-icon" sizes="76x76"/>
<link href="storefront/view/default/image/apple-touch-icon-120x120.png" rel="apple-touch-icon" sizes="120x120"/>
<link href="storefront/view/default/image/apple-touch-icon-152x152.png" rel="apple-touch-icon" sizes="152x152"/>
<link href="storefront/view/default/image/apple-touch-icon-precomposed.png" rel="apple-touch-icon-precomposed"/>


<link href='//fonts.googleapis.com/css?family=Open+Sans:400,300italic,400italic,600,600italic' rel='stylesheet' type='text/css'/>
<link href='//fonts.googleapis.com/css?family=Crete+Round' rel='stylesheet' type='text/css'/>
<link href="storefront/view/default/stylesheet/style.css" rel="stylesheet" type='text/css'/>


<!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
<!--[if lt IE 9]>
      <script type="text/javascript" src="//html5shim.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
<!-- fav -->


<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
<script type="text/javascript">if(typeof jQuery=='undefined'){var include='<script type="text/javascript" src="storefront/view/default/javascript/jquery-1.11.0.min.js"><\/script>';document.write(include);}</script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery-migrate-1.2.1.min.js"></script>

<script type="text/javascript" src="storefront/view/default/javascript/common.js"></script>

<script type="text/javascript">$(document).on('click','a.productcart',function(){var item=$(this);if(item.attr('href')&&item.attr('href')!='#'){return true;}
if(item.attr('data-id')){$.ajax({url:'http://192.168.56.102/index.php?rt=r/product/product/addToCart',type:'GET',dataType:'json',data:{product_id:item.attr('data-id')},success:function(data){var alert_msg='<div class="added_to_cart pull-right"> \
                    		<a href="http://192.168.56.102/index.php?rt=checkout/cart" title="Added to cart"> \
                    		<i class="fa fa-check"></i></a> \
                    		</div>';item.closest('.thumbnail .pricetag').prepend(alert_msg);$('.nav.topcart .dropdown-toggle span').first().html(data.item_count);$('.nav.topcart .dropdown-toggle .cart_total').html(data.total);if($('#top_cart_product_list')){$('#top_cart_product_list').html(data.cart_details);};}});}
return false;});$(document).on('click','a.call_to_order',function(){goTo('http://192.168.56.102/index.php?rt=content/contact');return false;});function search_submit(){var url='http://192.168.56.102/index.php?rt=product/search';var filter_keyword=$('#filter_keyword').val();if(filter_keyword){url+='&keyword='+encodeURIComponent(filter_keyword);}
var filter_category_id=$('#filter_category_id').attr('value');if(filter_category_id){url+='&category_id='+filter_category_id;}
location=url;return false;}</script>

</head>
<body>
<div class="container-fixed" style="max-width: 100%">

<header>
<div class="headerstrip navbar navbar-inverse" role="navigation">
	<div class="container-fluid">
	  <div class="navbar-header header-logo">
	    <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target=".navbar-collapse">
	      <span class="sr-only"></span>
	      <span class="icon-bar"></span>
	      <span class="icon-bar"></span>
	      <span class="icon-bar"></span>
	    </button>
	    	    <a class="logo" href="http://192.168.56.102/"><script pagespeed_no_defer="">//<![CDATA[
(function(){var g=this,h=function(b,d){var a=b.split("."),c=g;a[0]in c||!c.execScript||c.execScript("var "+a[0]);for(var e;a.length&&(e=a.shift());)a.length||void 0===d?c[e]?c=c[e]:c=c[e]={}:c[e]=d};var l=function(b){var d=b.length;if(0<d){for(var a=Array(d),c=0;c<d;c++)a[c]=b[c];return a}return[]};var m=function(b){var d=window;if(d.addEventListener)d.addEventListener("load",b,!1);else if(d.attachEvent)d.attachEvent("onload",b);else{var a=d.onload;d.onload=function(){b.call(this);a&&a.call(this)}}};var n,p=function(b,d,a,c,e){this.f=b;this.h=d;this.i=a;this.c=e;this.e={height:window.innerHeight||document.documentElement.clientHeight||document.body.clientHeight,width:window.innerWidth||document.documentElement.clientWidth||document.body.clientWidth};this.g=c;this.b={};this.a=[];this.d={}},q=function(b,d){var a,c,e=d.getAttribute("pagespeed_url_hash");if(a=e&&!(e in b.d))if(0>=d.offsetWidth&&0>=d.offsetHeight)a=!1;else{c=d.getBoundingClientRect();var f=document.body;a=c.top+("pageYOffset"in window?window.pageYOffset:(document.documentElement||f.parentNode||f).scrollTop);c=c.left+("pageXOffset"in window?window.pageXOffset:(document.documentElement||f.parentNode||f).scrollLeft);f=a.toString()+","+c;b.b.hasOwnProperty(f)?a=!1:(b.b[f]=!0,a=a<=b.e.height&&c<=b.e.width)}a&&(b.a.push(e),b.d[e]=!0)};p.prototype.checkImageForCriticality=function(b){b.getBoundingClientRect&&q(this,b)};h("pagespeed.CriticalImages.checkImageForCriticality",function(b){n.checkImageForCriticality(b)});h("pagespeed.CriticalImages.checkCriticalImages",function(){r(n)});var r=function(b){b.b={};for(var d=["IMG","INPUT"],a=[],c=0;c<d.length;++c)a=a.concat(l(document.getElementsByTagName(d[c])));if(0!=a.length&&a[0].getBoundingClientRect){for(c=0;d=a[c];++c)q(b,d);a="oh="+b.i;b.c&&(a+="&n="+b.c);if(d=0!=b.a.length)for(a+="&ci="+encodeURIComponent(b.a[0]),c=1;c<b.a.length;++c){var e=","+encodeURIComponent(b.a[c]);131072>=a.length+e.length&&(a+=e)}b.g&&(e="&rd="+encodeURIComponent(JSON.stringify(s())),131072>=a.length+e.length&&(a+=e),d=!0);t=a;if(d){c=b.f;b=b.h;var f;if(window.XMLHttpRequest)f=new XMLHttpRequest;else if(window.ActiveXObject)try{f=new ActiveXObject("Msxml2.XMLHTTP")}catch(k){try{f=new ActiveXObject("Microsoft.XMLHTTP")}catch(u){}}f&&(f.open("POST",c+(-1==c.indexOf("?")?"?":"&")+"url="+encodeURIComponent(b)),f.setRequestHeader("Content-Type","application/x-www-form-urlencoded"),f.send(a))}}},s=function(){var b={},d=document.getElementsByTagName("IMG");if(0==d.length)return{};var a=d[0];if(!("naturalWidth"in a&&"naturalHeight"in a))return{};for(var c=0;a=d[c];++c){var e=a.getAttribute("pagespeed_url_hash");e&&(!(e in b)&&0<a.width&&0<a.height&&0<a.naturalWidth&&0<a.naturalHeight||e in b&&a.width>=b[e].k&&a.height>=b[e].j)&&(b[e]={rw:a.width,rh:a.height,ow:a.naturalWidth,oh:a.naturalHeight})}return b},t="";h("pagespeed.CriticalImages.getBeaconData",function(){return t});h("pagespeed.CriticalImages.Run",function(b,d,a,c,e,f){var k=new p(b,d,a,e,f);n=k;c&&m(function(){window.setTimeout(function(){r(k)},0)})});})();pagespeed.CriticalImages.Run('/mod_pagespeed_beacon','http://192.168.56.102/index.php','Ez0F1MDYS1',true,false,'-a8Jgo7wJr0');
//]]></script><img src="resources/image/18/73/3.png" title="Web Store Name" alt="Web Store Name" pagespeed_url_hash="2023836573" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>
	    	  </div>
	  <div class="navbar-collapse collapse">
	  	<div class="navbar-right headerstrip_blocks">
	  	    <div class="block_1"></div>
	  	    <div class="block_2"><div id="customernav" class="navbar">
	<ul class="nav navbar-nav main_menu" id="customer_menu_top">
		<li><a href="https://192.168.56.102:443/index.php?rt=account/login&session_id=a9rabhh53osheuds6q4gjn5lr4">Login or register</a></li>
	</ul>
</div></div>
	  	    <div class="block_3"><div class="topnavbar navbar" id="topnav">
	<span class="sr-only">Main Menu</span>
 	<ul id="main_menu_top" class="nav navbar-nav main_menu">
		    	    <li data-id="menu_specials" class="dropdown "><a class="top menu_specials" href="https://192.168.56.102:443/index.php?rt=product/special&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-tag"></i>&nbsp;<span class="menu_text">Specials</span></a></li>
<li data-id="menu_account" class="dropdown "><a class="top menu_account" href="https://192.168.56.102:443/index.php?rt=account/account&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-user"></i>&nbsp;<span class="menu_text">Account</span></a>
<ul class='sub_menu dropdown-menu'>
<li data-id="menu_login" class="dropdown "><a class="sub menu_login" href="https://192.168.56.102:443/index.php?rt=account/login&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-user"></i>&nbsp;<span class="menu_text">Login</span></a></li>
</ul>
</li>
<li data-id="menu_cart" class="dropdown "><a class="top menu_cart" href="https://192.168.56.102:443/index.php?rt=checkout/cart&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-shopping-cart"></i>&nbsp;<span class="menu_text">Cart</span></a></li>
<li data-id="menu_checkout" class="dropdown "><a class="top menu_checkout" href="https://192.168.56.102:443/index.php?rt=checkout/shipping&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-money"></i>&nbsp;<span class="menu_text">Checkout</span></a></li>
	</ul>
</div></div>
	  	    <div class="block_4"><form id="search_form" class="form-search top-search">
    <input type="hidden" name="filter_category_id" id="filter_category_id" value="0"/>
    <div class="btn-group search-bar">
    	<input type="text" id="filter_keyword" name="filter_keyword" autocomplete="off" class="pull-left input-medium search-query dropdown-toggle" placeholder="Search Keywords" value="" data-toggle="dropdown"/>
    	 <div class="button-in-search" title="Go"><i class="fa fa-search"></i></div>
        	<ul class="dropdown dropdown-menu col-md-2 noclose">
    		<!-- dropdown menu links -->
    		<li class="active"><a id="category_selected">All Categories</a></li>
    		<li class="divider"></li>
			<li>
				<ul id="search-category">
									<li><a id="category_0">All Categories</a></li>
									<li><a id="category_68">Apparel &amp; accessories</a></li>
									<li><a id="category_36">Makeup</a></li>
									<li><a id="category_43">Skincare</a></li>
									<li><a id="category_49">Fragrance</a></li>
									<li><a id="category_58">Men</a></li>
									<li><a id="category_52">Hair Care</a></li>
									<li><a id="category_65">Books</a></li>
								</ul>
			</li>
    	</ul>
        </div>
</form></div>
	  	</div>
	   </div><!--/.navbar-collapse -->
	</div>         
</div>
<div class="container-fluid">
    <div class="col-md-12 headerdetails">
    	<!-- header blocks placeholder -->
    	<div class="block_5"></div>			
    	<div class="block_6"><ul class="nav language pull-left">
  <li class="dropdown hover">
  
	  
	  
		<a class="dropdown-toggle" data-toggle=""><span><span class="label label-orange font14">$</span> US Dollar</span><b class="caret"></b></a>
	    
    <ul class="dropdown-menu currency">
  
      <li>
      	<a href="http://192.168.56.102/index.php?rt=index/home&amp;currency=EUR">€ Euro</a>
      </li>
  
      <li>
      	<a href="http://192.168.56.102/index.php?rt=index/home&amp;currency=GBP">£ Pound Sterling</a>
      </li>
  
      <li>
      	<a href="http://192.168.56.102/index.php?rt=index/home&amp;currency=USD">$ US Dollar</a>
      </li>
    </ul>
  </li>
</ul>
</div>
    	<div class="block_7"><ul class="nav topcart pull-left">
    <li class="dropdown hover"> 
        <a href="http://192.168.56.102/index.php?rt=checkout/cart" class="dropdown-toggle"><i class="fa fa-shopping-cart fa-fw"></i>&nbsp;&nbsp;<span class="label label-orange font14">0</span> Items - <span class="cart_total">$0.00</span> <b class="caret"></b></a>
        <ul class="dropdown-menu topcartopen ">
            <li>
				<div id="top_cart_product_list">
				<div class="products">
<table>
	<tbody>
								                	
	</tbody>
</table>
</div>
<table class="totals pull-right mr20">
	<tbody>
			<tr>
			<td><span class="cart_block_total"><b>Sub-Total:</b></span></td>
			<td><span class="cart_block_total">$0.00</span></td>
		</tr>
			<tr>
			<td><span class="cart_block_total"><b>Total:</b></span></td>
			<td><span class="cart_block_total">$0.00</span></td>
		</tr>
		</tbody>
</table>				</div>
					
				<div class="buttonwrap">
				    				    <a class="btn btn-orange btn-xs pull-left" href="http://192.168.56.102/index.php?rt=checkout/cart"><i class="fa fa-shopping-cart fa-fw"></i> View Cart</a>&nbsp;&nbsp;
				    <a class="btn btn-orange btn-xs pull-right" href="http://192.168.56.102/index.php?rt=checkout/shipping"><i class="fa fa-pencil fa-fw"></i>  Checkout</a>
				    				</div>
            </li>
        </ul>
    </li>
</ul></div>
    	<div class="block_8"><div class="header_block">
			      <div class="social_icons">
        <a href="http://www.facebook.com/AbanteCart" target="_blank" title="Facebook" class="facebook">Facebook</a>
        <a href="https://twitter.com/abantecart" target="_blank" title="Twitter" class="twitter">Twitter</a>
        <a href="#" title="Linkedin" class="linkedin">Linkedin</a>
        <a href="#" title="rss" class="rss">rss</a>
        <a href="#" target="_blank" title="Googleplus" class="googleplus">Googleplus</a>
        <a href="#" target="_blank" title="Skype" class="skype">Skype</a>
        <a href="#" target="_blank" title="Flickr" class="flickr">Flickr</a>
      </div>
		</div></div>
    	<!-- header blocks placeholder (EOF) -->
    </div>
</div>

</header>

<!-- header_bottom blocks placeholder -->
	<div class="container-fluid">
	    	<div class="col-md-12">
<section id="categorymenu">
    <nav class="subnav">
    	<ul class="nav-pills categorymenu">
    		<li><a class="active menu_home" href="http://192.168.56.102/index.php?rt=index/home">Home</a>

    			<div>
    				<ul id="main_menu" class="nav">
    					    					<!-- Top Nav Start -->
    					<li data-id="menu_specials" class="dropdown "><a class="top menu_specials" href="https://192.168.56.102:443/index.php?rt=product/special&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-tag"></i>&nbsp;<span class="menu_text">Specials</span></a></li>
<li data-id="menu_account" class="dropdown "><a class="top menu_account" href="https://192.168.56.102:443/index.php?rt=account/account&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-user"></i>&nbsp;<span class="menu_text">Account</span></a>
<ul class='sub_menu dropdown-menu'>
<li data-id="menu_login" class="dropdown "><a class="sub menu_login" href="https://192.168.56.102:443/index.php?rt=account/login&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-user"></i>&nbsp;<span class="menu_text">Login</span></a></li>
</ul>
</li>
<li data-id="menu_cart" class="dropdown "><a class="top menu_cart" href="https://192.168.56.102:443/index.php?rt=checkout/cart&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-shopping-cart"></i>&nbsp;<span class="menu_text">Cart</span></a></li>
<li data-id="menu_checkout" class="dropdown "><a class="top menu_checkout" href="https://192.168.56.102:443/index.php?rt=checkout/shipping&session_id=g50tcgbeicl2d5nj3t9k62tlq5"><i class="fa fa-money"></i>&nbsp;<span class="menu_text">Checkout</span></a></li>
    				</ul>
    			</div>
    		</li>
    		  		
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=68">&nbsp;&nbsp;Apparel &amp; accessories</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=68_69">&nbsp;&nbsp;&nbsp;&nbsp;Shoes</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/77/new_ladies_red1_jpg-100216-120x120.jpg" alt="Shoes" title="Shoes" pagespeed_url_hash="2963718076" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=68_70">&nbsp;&nbsp;&nbsp;&nbsp;T-shirts</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_3_jpg-100243-120x120.jpg" alt="T-shirts" title="T-shirts" pagespeed_url_hash="3705276265" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_3a_jpg-100244-120x120.jpg" alt="Apparel &amp; accessories" title="Apparel &amp; accessories" pagespeed_url_hash="627810081" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_3a_jpg-100244-120x120.jpg" alt="Apparel &amp; accessories" title="Apparel &amp; accessories" pagespeed_url_hash="627810081" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36">&nbsp;&nbsp;Makeup</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36_40">&nbsp;&nbsp;&nbsp;&nbsp;Cheeks</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_07_jpg-100133-120x120.jpg" alt="Cheeks" title="Cheeks" pagespeed_url_hash="3052266180" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36_39">&nbsp;&nbsp;&nbsp;&nbsp;Eyes</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_47_png-100136-120x120.png" alt="Eyes" title="Eyes" pagespeed_url_hash="1269902683" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36_38">&nbsp;&nbsp;&nbsp;&nbsp;Face</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_05_jpg-100132-120x120.jpg" alt="Face" title="Face" pagespeed_url_hash="2551043389" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36_41">&nbsp;&nbsp;&nbsp;&nbsp;Lips</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_08_3_jpg-100134-120x120.jpg" alt="Lips" title="Lips" pagespeed_url_hash="1438652528" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36_42">&nbsp;&nbsp;&nbsp;&nbsp;Nails</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_10_2_jpg-100135-120x120.jpg" alt="Nails" title="Nails" pagespeed_url_hash="2829818983" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=36_37">&nbsp;&nbsp;&nbsp;&nbsp;Value Sets</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_11_2_jpg-100137-120x120.jpg" alt="Value Sets" title="Value Sets" pagespeed_url_hash="3637165932" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_04_jpg-100124-120x120.jpg" alt="Makeup" title="Makeup" pagespeed_url_hash="499685238" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_04_jpg-100124-120x120.jpg" alt="Makeup" title="Makeup" pagespeed_url_hash="499685238" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=43">&nbsp;&nbsp;Skincare</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=43_47">&nbsp;&nbsp;&nbsp;&nbsp;Eyes</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_46_jpg-100143-120x120.jpg" alt="Eyes" title="Eyes" pagespeed_url_hash="1181843664" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=43_46">&nbsp;&nbsp;&nbsp;&nbsp;Face</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_42_jpg-100142-120x120.jpg" alt="Face" title="Face" pagespeed_url_hash="1263920051" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=43_45">&nbsp;&nbsp;&nbsp;&nbsp;Gift Ideas &amp; Sets</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/73/demo_product_37_jpg-100145-120x120.jpg" alt="Gift Ideas &amp; Sets" title="Gift Ideas &amp; Sets" pagespeed_url_hash="2489255719" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=43_48">&nbsp;&nbsp;&nbsp;&nbsp;Hands &amp; Nails</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/73/demo_product_49_1_png-100146-120x120.png" alt="Hands &amp; Nails" title="Hands &amp; Nails" pagespeed_url_hash="248411129" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=43_44">&nbsp;&nbsp;&nbsp;&nbsp;Sun</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/73/demo_product_18_jpg-100144-120x120.jpg" alt="Sun" title="Sun" pagespeed_url_hash="215865151" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_15_jpg-100125-120x120.jpg" alt="Skincare" title="Skincare" pagespeed_url_hash="1741167123" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_15_jpg-100125-120x120.jpg" alt="Skincare" title="Skincare" pagespeed_url_hash="1741167123" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=49">&nbsp;&nbsp;Fragrance</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=49_51">&nbsp;&nbsp;&nbsp;&nbsp;Men</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_30_2_jpg-100129-120x120.jpg" alt="Men" title="Men" pagespeed_url_hash="2252156842" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=49_50">&nbsp;&nbsp;&nbsp;&nbsp;Women</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_34_jpg-100128-120x120.jpg" alt="Women" title="Women" pagespeed_url_hash="1767261544" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_14_2_jpg-100126-120x120.jpg" alt="Fragrance" title="Fragrance" pagespeed_url_hash="2243356590" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_14_2_jpg-100126-120x120.jpg" alt="Fragrance" title="Fragrance" pagespeed_url_hash="2243356590" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=58">&nbsp;&nbsp;Men</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=58_63">&nbsp;&nbsp;&nbsp;&nbsp;Body &amp; Shower</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_27_jpg-100141-120x120.jpg" alt="Body &amp; Shower" title="Body &amp; Shower" pagespeed_url_hash="2118898423" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=58_59">&nbsp;&nbsp;&nbsp;&nbsp;Fragrance Sets</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_40_2_jpg-100138-120x120.jpg" alt="Fragrance Sets" title="Fragrance Sets" pagespeed_url_hash="3512412917" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=58_61">&nbsp;&nbsp;&nbsp;&nbsp;Pre-Shave &amp; Shaving</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_29_jpg-100140-120x120.jpg" alt="Pre-Shave &amp; Shaving" title="Pre-Shave &amp; Shaving" pagespeed_url_hash="451077276" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=58_60">&nbsp;&nbsp;&nbsp;&nbsp;Skincare</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_44_jpg-100139-120x120.jpg" alt="Skincare" title="Skincare" pagespeed_url_hash="3638850189" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/79/demo_product31_png-100249-120x120.png" alt="Men" title="Men" pagespeed_url_hash="1618927639" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/79/demo_product31_png-100249-120x120.png" alt="Men" title="Men" pagespeed_url_hash="1618927639" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=52">&nbsp;&nbsp;Hair Care</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=52_54">&nbsp;&nbsp;&nbsp;&nbsp;Conditioner</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_23_jpg-100131-120x120.jpg" alt="Conditioner" title="Conditioner" pagespeed_url_hash="2947039608" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=52_53">&nbsp;&nbsp;&nbsp;&nbsp;Shampoo</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/72/demo_product_24_jpg-100130-120x120.jpg" alt="Shampoo" title="Shampoo" pagespeed_url_hash="1570868050" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_23_jpg-100123-120x120.jpg" alt="Hair Care" title="Hair Care" pagespeed_url_hash="604031868" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/71/demo_product_23_jpg-100123-120x120.jpg" alt="Hair Care" title="Hair Care" pagespeed_url_hash="604031868" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    				<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=65">&nbsp;&nbsp;Books</a>
    					    					    						<!-- Subcategories -->
    						<div class="subcategories">
    							<ul>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=65_66">&nbsp;&nbsp;&nbsp;&nbsp;Audio CD</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/76/cdaudio_png-100199-120x120.png" alt="Audio CD" title="Audio CD" pagespeed_url_hash="3491539174" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								     									<li><a href="http://192.168.56.102/index.php?rt=product/category&amp;path=65_67">&nbsp;&nbsp;&nbsp;&nbsp;Paperback</a>
    									<img class="sub_cat_image" style="display:none; width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/76/paper2_jpg-100202-120x120.jpg" alt="Paperback" title="Paperback" pagespeed_url_hash="4203199312" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								    							</ul>
    							    								<ul>
    									<li class="parent_cat_image" style="display:none">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/76/book2_png-100200-120x120.png" alt="Books" title="Books" pagespeed_url_hash="711938215" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    									<li class="cat_image">
										    <img class="root_cat_image" style="display:block;  width: 120px; height: 120px;" src="http://192.168.56.102/image/thumbnails/18/76/book2_png-100200-120x120.png" alt="Books" title="Books" pagespeed_url_hash="711938215" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
    									</li>
    								</ul>
    							    						</div>
    					    				</li>
    			    		    	</ul>
    </nav>
</section>	</div>
	<div class="col-md-12">
<section class="slider">
  <div class="banner_conteiner">  		
	<div class="banner banner_fallback" data-banner-id="18"><a href="http://192.168.56.102/index.php?rt=r/extension/banner_manager/click&amp;banner_id=18"><img src="http://192.168.56.102/image/thumbnails/18/76/banner_fallback_jpg-100194-x.jpg" title="Fall back banner for small screen resolutions" alt="Fall back banner for small screen resolutions" pagespeed_url_hash="1415933043" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"></a></div>		<div id="banner_slides">		
		
			<div class="oneByOne_item banner" data-banner-id="8">
	<p>
	<img alt="" class="wp1_3 slide1_bot" src="storefront/view/default/image/banner_image_1.png" pagespeed_url_hash="3559412474" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/> <span class="txt1">HTML5 Responsive Storefront to look great on</span> <span class="txt2">ALL Screen Sizes</span> <span class="txt3 short">Natively responsive template implemented with bootstrap library and HTML5. Will look good on most mobile devices and tablets.</span> <span class="txt4 txt4up"><a class="btn btn-wht" href="">Try on your device!</a></span></p>
		
			</div>
		
			<div class="oneByOne_item banner" data-banner-id="9">
	<p>
	<img alt="" class="wp1_3 wp1_left slide2_bot" src="storefront/view/default/image/banner_image_2.png" pagespeed_url_hash="3853912395" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/> <span class="txt1 blue txt_right2">Highly flexible layout on any page</span> <span class="txt2 blue txt_right2">SEO Friendly</span> <span class="txt2 blue txt_right2">Fast Loading</span> <span class="txt4 txt_right2 txt4up"><a class="btn btn-wht" href="#">Try Now!</a></span></p>
		
			</div>
		
			<div class="oneByOne_item banner" data-banner-id="10">
	<p>
	<img alt="" class="wp1_3 slide2_bot" src="storefront/view/default/image/banner_image_3.png" pagespeed_url_hash="4148412316" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/> <span class="txt1">Feature rich with smart UI</span> <span class="txt2">Easy &amp; fun to manage</span> <span class="txt3">Feature reach shopping cart application right out of the box. Standard features allow to set up complete eCommerce site with all the tools needed to sell products online.</span> <span class="txt4"><a class="btn btn-wht" href="#">Install Now!</a></span></p>
		
			</div>
		
			<div class="oneByOne_item banner" data-banner-id="11">
	<p>
	<img alt="" class="wp1_3 slide3_bot" src="storefront/view/default/image/banner_image_4.png" pagespeed_url_hash="147944941" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/> <span class="txt1 blue">Stay in control</span> <span class="txt2 blue">Easy updates</span> <span class="txt3 short">Upgrade right from admin. Backward supportability in upgrades and automatic backups. Easy extension download with one step installation.</span> <span class="txt4 txt4up"><a class="btn btn-wht" href="#">Get Yours!</a></span></p>
		
			</div>
		
			<div class="oneByOne_item banner" data-banner-id="17">
	<p>
	<img alt="" class="wp1_3 slide2_bot" src="storefront/view/default/image/banner_image_5.png" pagespeed_url_hash="442444862" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/> <span class="txt1">Application and data security</span> <span class="txt2">Secure Solution</span> <span class="txt3">Very secure solution with up to date industry security practices and inline with PCI compliance. Customer information protection with data encryption</span> <span class="txt4"><a class="btn btn-wht" href="#">Install Now!</a></span></p>
		
			</div>
			
				
		</div>    
  </div>
</section>	</div>
	</div>
<!-- header_bottom blocks placeholder -->

<div id="maincontainer">

  

	<div class="container-fluid">
		
				<div class="col-md-12 col-xs-12 mt20">
				<!-- content top blocks placeholder -->
		<section class="row mt40">
	<div class="container-fluid">
			<section class="row promo_section">
	<div class="col-md-3 col-xs-6 promo_block">
		<div class="promo_icon"><i class="fa fa-truck fa-fw"></i></div>
		<div class="promo_text">
			<h2>
				Free shipping</h2>
			All over in world over $200
		</div>
	</div>
	<div class="col-md-3 col-xs-6 promo_block">
		<div class="promo_icon"><i class="fa fa-money fa-fw"></i></div>
		<div class="promo_text">
			<h2>
				Easy Payment</h2>
			Payment Gateway support</div>
	</div>
	<div class="col-md-3 col-xs-6 promo_block">
		<div class="promo_icon"><i class="fa fa-clock-o fa-fw"></i></div>
		<div class="promo_text">
			<h2>
				24hrs Shipping</h2>
			For All US States</div>
	</div>
	<div class="col-md-3 col-xs-6 promo_block">
		<div class="promo_icon"><i class="fa fa-tags fa-fw"></i></div>
		<div class="promo_text">
			<h2>
				Large Variety</h2>
			50,000+ Products</div>
	</div>
	</section>			</div>
</section>
		<!-- content top blocks placeholder (EOF) -->
				
		<div class="">
		<section class="contentpanel">
	<div class="welcome_msg">
		Welcome to web store!	</div>
</section>		</div>
		 
				<!-- content bottom blocks placeholder -->
		<section id="featured" class="row mt40">
	<div class="container-fluid">
				<div class="block_frame block_frame_featured" id="block_frame_featured_1769">
			<h1 class="heading1"><span class="maintext">Featured</span><span class="subtext">See Our Most featured Products</span></h1>
			
			<ul class="thumbnails list-inline">
				<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=50" title="Skinsheen Bronzer Stick">Skinsheen Bronzer Stick</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=50"><img src="http://192.168.56.102/image/thumbnails/18/6f/demo_product01_jpg-100089-250x250.jpg" width="250" alt="" pagespeed_url_hash="3839584940" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=50">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=50#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="50" href="#" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$29.50</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51" title="BeneFit Girl Meets Pearl">BeneFit Girl Meets Pearl</a>
					</div>
				</div>
				<div class="thumbnail">
											<span class="sale tooltip-test"></span>
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51"><img src="http://192.168.56.102/image/thumbnails/18/6b/demo_product02_jpg-100026-250x250.jpg" width="250" alt="" pagespeed_url_hash="2365583694" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="51" href="#" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="pricenew">$19.00</div>
									<div class="priceold">$30.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=52" title="Benefit Bella Bamba">Benefit Bella Bamba</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=52"><img src="http://192.168.56.102/image/thumbnails/18/6b/demo_product02_3_jpg-100029-250x250.jpg" width="250" alt="" pagespeed_url_hash="3717182641" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=52">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=52#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="52" href="#" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$28.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=54" title="L'EXTRÊME Instant Extensions Lengthening Mascara">L'EXTRÊME Instant Extensions Lengthening Mascara</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=54"><img src="http://192.168.56.102/image/thumbnails/18/79/lancome_mascara_jpg-100250-250x250.jpg" width="250" alt="" pagespeed_url_hash="3990463478" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=54">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=54#review"><img src='storefront/view/default/image/stars_5.png' alt='5 out of 5 Stars!' pagespeed_url_hash="3154581057" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="54" href="#" class="btn call_to_order">Call To Order&nbsp;&nbsp;
								<i class="fa fa-phone"></i>
							</a>
														<div class="price">
																	<div class="oneprice">$25.00</div>
															</div>
						</div>
									</div>
			</li>
		</ul>
					</div>
		</div>
</section>
<div class="sep"></div>
  
<section id="latest" class="row mt40">
	<div class="container-fluid">
				<div class="block_frame block_frame_latest" id="block_frame_latest_1770">
			<h1 class="heading1"><span class="maintext">Latest Products</span><span class="subtext">See New Products</span></h1>
			
			<ul class="thumbnails list-inline">
				<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=122" title="Mens Fine Cotton Giraffe Polo Shirts">Mens Fine Cotton Giraffe Polo Shirts</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=122"><img src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_4_jpg-100248-250x250.jpg" width="250" alt="" pagespeed_url_hash="2554739949" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=122">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=122#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="122" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=122" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$21.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=121" title="Designer Men Casual Formal Double Cuffs Grandad Band Collar Shirt Elegant Tie">Designer Men Casual Formal Double Cuffs Grandad Band Collar Shirt Elegant Tie</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=121"><img src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_3_jpg-100243-250x250.jpg" width="250" alt="" pagespeed_url_hash="1718746989" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=121">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=121#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="121" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=121" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$32.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=120" title="Jersey Cotton Striped Polo Shirt">Jersey Cotton Striped Polo Shirt</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=120"><img src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_2_jpg-100242-250x250.jpg" width="250" alt="" pagespeed_url_hash="925874609" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=120">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=120#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="120" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=120" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$6.75</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=119" title="Fruit of the Loom T-Shirts 5 Pack - Super Premium">Fruit of the Loom T-Shirts 5 Pack - Super Premium</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=119"><img src="http://192.168.56.102/image/thumbnails/18/79/t_shirt_jpg-100241-250x250.jpg" width="250" alt="" pagespeed_url_hash="2711925803" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=119">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=119#review"><img src='storefront/view/default/image/stars_5.png' alt='5 out of 5 Stars!' pagespeed_url_hash="3154581057" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="119" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=119" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$9.99</div>
															</div>
						</div>
									</div>
			</li>
		</ul>
					</div>
		</div>
</section><div class="sep"></div>
  
<section id="bestseller" class="row mt40">
	<div class="container-fluid">
				<div class="block_frame block_frame_bestsellers" id="block_frame_bestsellers_1771">
			<h1 class="heading1"><span class="maintext">Bestsellers</span><span class="subtext">See Best Selling Products</span></h1>
			
			<ul class="thumbnails list-inline">
				<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=104" title="Calvin Klein Obsession For Women EDP Spray">Calvin Klein Obsession For Women EDP Spray</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=104"><img src="http://192.168.56.102/image/thumbnails/18/74/demo_product54_1_jpg-100160-250x250.jpg" width="250" alt="" pagespeed_url_hash="1723765039" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=104">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=104#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="104" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=104" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$49.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=69" title="Seaweed Conditioner">Seaweed Conditioner</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=69"><img src="http://192.168.56.102/image/thumbnails/18/6f/demo_product19_jpg-100082-250x250.jpg" width="250" alt="" pagespeed_url_hash="2953278542" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=69">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=69#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="69" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=69" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$19.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=78" title="ck IN2U Eau De Toilette Spray for Him">ck IN2U Eau De Toilette Spray for Him</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=78"><img src="http://192.168.56.102/image/thumbnails/18/70/demo_product28_jpg-100109-250x250.jpg" width="250" alt="" pagespeed_url_hash="2943775137" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=78">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=78#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="78" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=78" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$29.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=61" title="Color Design Eye Brightening All in One 5 Shadow &amp; Liner Palette">Color Design Eye Brightening All in One 5 Shadow &amp; Liner Palette</a>
					</div>
				</div>
				<div class="thumbnail">
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=61"><img src="http://192.168.56.102/image/thumbnails/18/6c/demo_product11_1_jpg-100032-250x250.jpg" width="250" alt="" pagespeed_url_hash="1695336894" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=61">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=61#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="61" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=61" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="oneprice">$48.00</div>
															</div>
						</div>
									</div>
			</li>
		</ul>
								</div>
	</div>
</section><div class="sep"></div>
  
<section id="special" class="row mt40">
    <div class="container-fluid">
		<div class="block_frame block_frame_special" id="block_frame_special_1772">
      <h1 class="heading1"><span class="maintext">Specials</span><span class="subtext">See Products On Sale</span></h1>

	<ul class="thumbnails list-inline">
				<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=65" title="Absolue Eye Precious Cells">Absolue Eye Precious Cells</a>
					</div>
				</div>
				<div class="thumbnail">
											<span class="sale tooltip-test"></span>
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=65"><img src="http://192.168.56.102/image/thumbnails/18/6a/demo_product15_jpg-100011-250x250.jpg" width="250" alt="" pagespeed_url_hash="2572375855" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=65">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=65#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="65" href="#" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="pricenew">$89.00</div>
									<div class="priceold">$105.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=80" title="Acqua Di Gio Pour Homme">Acqua Di Gio Pour Homme</a>
					</div>
				</div>
				<div class="thumbnail">
											<span class="sale tooltip-test"></span>
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=80"><img src="http://192.168.56.102/image/thumbnails/18/6a/demo_product30_jpg-100014-250x250.jpg" width="250" alt="" pagespeed_url_hash="3886441421" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=80">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=80#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="80" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=80" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="pricenew">$45.00</div>
									<div class="priceold">$59.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51" title="BeneFit Girl Meets Pearl">BeneFit Girl Meets Pearl</a>
					</div>
				</div>
				<div class="thumbnail">
											<span class="sale tooltip-test"></span>
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51"><img src="http://192.168.56.102/image/thumbnails/18/6b/demo_product02_jpg-100026-250x250.jpg" width="250" alt="" pagespeed_url_hash="2365583694" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=51#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="51" href="#" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="pricenew">$19.00</div>
									<div class="priceold">$30.00</div>
															</div>
						</div>
									</div>
			</li>
					<li class="col-md-3 col-sm-6 col-xs-12">
				<div class="fixed_wrapper">
					<div class="fixed">
						<a class="prdocutname" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=72" title="Brunette expressions Conditioner">Brunette expressions Conditioner</a>
					</div>
				</div>
				<div class="thumbnail">
											<span class="sale tooltip-test"></span>
															<a href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=72"><img src="http://192.168.56.102/image/thumbnails/18/6b/demo_product22_jpg-100031-250x250.jpg" width="250" alt="" pagespeed_url_hash="2473617326" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>

					<div class="shortlinks">
						<a class="details" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=72">View</a>
													<a class="compare" href="http://192.168.56.102/index.php?rt=product/product&amp;product_id=72#review">Write Review</a>
																	</div>
					<div class="blurb"></div>
											<div class="pricetag jumbotron">
							<span class="spiral"></span>
														<a data-id="72" href="#" class="productcart">Add to Cart							</a>
														<div class="price">
																	<div class="pricenew">$24.00</div>
									<div class="priceold">$27.00</div>
															</div>
						</div>
									</div>
			</li>
		</ul>
</div>
	</div>
</section><div class="sep"></div>
  
<section id="banner_banner_block_1773" class="banner mt20">
    <div class="container-fluid">
	<ul class="list-inline">	
<li class="mb10" data-banner-id="13"><a href="http://192.168.56.102/index.php?rt=r/extension/banner_manager/click&amp;banner_id=13"><img src="http://192.168.56.102/image/thumbnails/18/75/smbanner_jpg-100188-x.jpg" title="" alt="" pagespeed_url_hash="2000894158" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"></a></li><li class="mb10" data-banner-id="14"><a href="http://192.168.56.102/index.php?rt=r/extension/banner_manager/click&amp;banner_id=14"><img src="http://192.168.56.102/image/thumbnails/18/75/smbanner_jpg-100188-x.jpg" title="" alt="" pagespeed_url_hash="2000894158" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"></a></li><li class="mb10" data-banner-id="15"><a href="http://192.168.56.102/index.php?rt=r/extension/banner_manager/click&amp;banner_id=15"><img src="http://192.168.56.102/image/thumbnails/18/75/smbanner_jpg-100188-x.jpg" title="" alt="" pagespeed_url_hash="2000894158" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"></a></li><li class="mb10" data-banner-id="16"><a href="http://192.168.56.102/index.php?rt=r/extension/banner_manager/click&amp;banner_id=16"><img src="http://192.168.56.102/image/thumbnails/18/75/smbanner_jpg-100188-x.jpg" title="" alt="" pagespeed_url_hash="2000894158" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"></a></li>	</ul>
	</div>
</section><div class="sep"></div>
  
<!-- Popular Brands-->
<section id="popularbrands" class="container-fluid mt40">
    <div class="container-fluid">
		<div class="block_frame block_frame_listing_block" id="block_frame_listing_block_1774">
      <h1 class="heading1"><span class="maintext">Brands Scrolling List</span><span class="subtext"></span></h1>
    <div class="brandcarousalrelative">
    <ul id="brandcarousal">
	<li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=12"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_benefit_logo_black_jpg-100117-250x250.jpg" alt="Benefit" pagespeed_url_hash="2153699649" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=17"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_pantene_logo_jpg-100120-250x250.jpg" alt="Pantene" pagespeed_url_hash="3222622731" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=11"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_mac_logo_jpg-100118-250x250.jpg" alt="M·A·C" pagespeed_url_hash="136212546" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=15"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_lancome_logo_gif-100119-250x250.gif" alt="Lancôme" pagespeed_url_hash="4269189949" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=20"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/73/demo_mf_gucci_jpg-100153-250x250.jpg" alt="Gucci" pagespeed_url_hash="3623601379" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=19"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_armani_logo_gif-100122-250x250.gif" alt="Giorgio Armani" pagespeed_url_hash="1688719110" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=18"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_dove_logo_jpg-100121-250x250.jpg" alt="Dove" pagespeed_url_hash="3766580483" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=13"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_calvin_klein_jpg-100116-250x250.jpg" alt="Calvin Klein" pagespeed_url_hash="3428082392" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=14"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_Bvlgari_jpg-100115-250x250.jpg" alt="Bvlgari" pagespeed_url_hash="592063615" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li><li><div class="image"><a href="http://192.168.56.102/index.php?rt=product/manufacturer&amp;manufacturer_id=16"><img class="internal" src="http://192.168.56.102/image/thumbnails/18/71/mf_sephora_ba_logo_black_jpg-100114-250x250.jpg" alt="Sephora" pagespeed_url_hash="3490213735" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a></div><div style="clear: both;"></div></li>  
   </ul>
   <div class="clearfix"></div>
   <a id="prev" class="prev" href="#">&lt;</a>
   <a id="next" class="next" href="#">&gt;</a>  
   </div> 
	</div>
	</div>
</section>
<!-- End Popular Brands-->
<div class="sep"></div>
  
		<!-- content bottom blocks placeholder (EOF) -->
				</div>

			</div><!-- content container -->

</div>
<!-- /maincontainer -->


<!-- footer blocks placeholder -->
<div id="footer">
	<!-- Footer -->
<footer>
	<!-- footer blocks placeholder -->
	<section class="footersocial">
		<div class="container-fluid">
			<div class="row">
				<div class="col-md-3">
					<div class="footer_block">
		<div class="block_frame block_frame_html_block" id="block_frame_html_block_1775">
		<h2>About Us</h2>
				<p>
	AbanteCart is a free eCommerce solution for merchants to provide ability creating online business and sell products or services online. AbanteCart application is built and supported by experienced enthusiasts that are passionate about their work and contribution to rapidly evolving eCommerce industry. AbanteCart is more than just a shopping cart, it is rapidly growing eCommerce platform with many benefits.</p>
			</div>
</div>				</div>
				<div class="col-md-3">
					<div class="footer_block">
		<div class="block_frame block_frame_html_block" id="block_frame_html_block_1776">
		<h2>Contact Us</h2>
				<ul class="contact">	<li><span class="phone"> </span>+123 456 7890, +123 456 7890</li>	<li><span class="mobile"> </span>+123 456 7890, +123 456 78900</li>	<li><span class="email"> </span>help at abantecart.com</li>	<li><span class="email"> </span>help at abantecart.com</li></ul>			</div>
</div>				</div>
				<div class="col-md-3">
					<div class="footer_block">
		<div class="block_frame block_frame_html_block" id="block_frame_html_block_1777">
		<h2>Testimonials</h2>
				<div style="font-family: 'Open Sans', sans-serif;" class="flexslider" id="testimonialsidebar">
	<ul class="slides">
		<li>
			" I was working with many shopping carts, free and hosted for my clients. There is always something missing. In abantecart I find this gap to be much less. Interface is very easy to use and support is very responsive. This is considering its is free. Go abantecart go!"<br/>
			<span class="pull-left orange">By : TopShop on reviewcentre.com</span></li>
		<li>
			" Without a doubt the best cart I have used. The title says it all - abantecart is undoubtedly the best I have used. I'm not an expert in site setup, so something this great looking and easy to use is absolutely perfect ... "<br/>
			<span class="pull-left orange">By : johnstenson80 on venturebeat.com</span></li>
		<li>
			" Will not regret using this cart. All good is already mentioned, I want to add my experience with support. My problems with some configuration were resolved quick. Faster than paid shopping cart we had before."<br/>
			<span class="pull-left orange">By : shopper23 at bestshoppingcartreviews.com</span></li>
		<li>
			" Wow! Abante Cart is really a catch! What a nice experience it was for me. I mean, to have all these features so direct, so quick and easy was really essential for my website. I was able to add some features and a cart to my website in no time ..."<br/>
			<span class="pull-left orange">By : EcommerceSport at hotscripts.com</span></li>
		<li>
			" Love the cart. I installed it a while back and use it since when. Some features a hidden, but fun to discover them."<br/>
			<span class="pull-left orange">By : Liz Wattkins at shopping-cart-reviews.com</span></li>

	</ul>
</div>
			</div>
</div>				</div>
				<div class="col-md-3">
					<div class="footer_block">
<h2>Newsletter Signup</h2>
<section id="newslettersignup">
	<div class="pull-left newsletter">Sign up to Our Newsletter &amp; get attractive Offers by subscribing to our newsletters.</div>
	<div class="pull-right">
		<form id="subscribeFrm" role="form" action="https://192.168.56.102:443/index.php?rt=account/subscriber&amp;session_id=a9rabhh53osheuds6q4gjn5lr4" method="get" enctype="multipart/form-data">			<div class="input-group">
								<input type="hidden" name="rt" value="account/subscriber">
								<input type="text" placeholder="Subscribe to Newsletter" name="email" id="appendedInputButton" class="form-control">
				<span class="input-group-btn">
					<button class="btn btn-orange" type="submit">Subscribe</button>
				</span>
			</div>
		</form>
	</div>
</section>
</div>				</div>
			</div>
		</div>
	</section>

	<section class="footerlinks">
		<div class="container-fluid">
			<div class="pull-left">
				<div class="info">
	<ul class="info_links_footer">
								<li><div class="dropdown">
					<a href="http://192.168.56.102/index.php?rt=content/content&amp;content_id=1">About Us</a>
							</div></li>
							<li><div class="dropdown">
					<a href="http://192.168.56.102/index.php?rt=content/content&amp;content_id=2">Privacy Policy</a>
							</div></li>
							<li><div class="dropdown">
					<a href="http://192.168.56.102/index.php?rt=content/content&amp;content_id=3">Return Policy</a>
							</div></li>
							<li><div class="dropdown">
					<a href="http://192.168.56.102/index.php?rt=content/content&amp;content_id=4">Shipping</a>
							</div></li>
					<li><a href="http://192.168.56.102/index.php?rt=content/contact">Contact</a></li>
		<li><a href="http://192.168.56.102/index.php?rt=content/sitemap">Sitemap</a></li>
					<li><a href="https://192.168.56.102:443/index.php?rt=account/login&amp;session_id=a9rabhh53osheuds6q4gjn5lr4">Login</a></li>
					</ul>
</div>			</div>
			<div class="pull-right">
				<div class="footer_block">
			      <div class="social_icons">
        <a href="http://www.facebook.com/AbanteCart" target="_blank" title="Facebook" class="facebook">Facebook</a>
        <a href="https://twitter.com/abantecart" target="_blank" title="Twitter" class="twitter">Twitter</a>
        <a href="#" title="Linkedin" class="linkedin">Linkedin</a>
        <a href="#" title="rss" class="rss">rss</a>
        <a href="#" target="_blank" title="Googleplus" class="googleplus">Googleplus</a>
        <a href="#" target="_blank" title="Skype" class="skype">Skype</a>
        <a href="#" target="_blank" title="Flickr" class="flickr">Flickr</a>
      </div>
		</div>			</div>
		</div>
	</section>
	<section class="copyrightbottom align_center">
		<div class="container-fluid">
			<div class="pull-left mt5">
				<div class="b_block flt_right payment">
	<img alt="payments" src="storefront/view/default/image/payment.gif" pagespeed_url_hash="1476539" onload="pagespeed.CriticalImages.checkImageForCriticality(this);">
</div>			</div>
			<div class="pull-right align_center">
				Powered By  <a href="https://bitnami.com/stack/abantecart" onclick="window.open(this.href);return false;" title="Ideal OpenSource E-commerce Solution">Bitnami AbanteCart Stack</a>				<br/>
	    		Web Store Name &copy; 2016				
			</div>
			<div class="pull-right mr20 mt5">
				<div class="b_block flt_right payment">
<a href="http://www.abantecart.com/contribute-to-abantecart" target="_new"><img src="image/conrib_btn_sm.png" border="0" alt="Support AbanteCart eCommerce" pagespeed_url_hash="767914362" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/></a>
</div>			</div>
		</div>
	</section>
	<a id="gotop" href="#">Back to top</a>
</footer>
</div><!-- container-fixed -->

<div id="msgModal" class="modal fade">
<div class="modal-dialog">
<div class="modal-content">
  <div class="modal-header">
    <button type="button" class="close callback-btn" data-dismiss="modal" aria-hidden="true">&times;</button>
    <h3></h3>
  </div>
  <div class="modal-body">
  </div>
</div>
</div>  
</div>

<!--
AbanteCart is open source software and you are free to remove the Powered By AbanteCart if you want, but its generally accepted practise to make a small donatation.
Please donate http://www.abantecart.com/donate
//-->

<!-- Placed at the end of the document so the pages load faster -->
<script type="text/javascript" src="storefront/view/default/javascript/bootstrap.min.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/respond.min.js"></script>
<script type="text/javascript" defer src="storefront/view/default/javascript/jquery.flexslider.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/easyzoom.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery.validate.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery.carouFredSel-6.1.0-packed.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery.mousewheel.min.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery.touchSwipe.min.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery.ba-throttle-debounce.min.js"></script>
<script type="text/javascript" src="storefront/view/default/javascript/jquery.onebyone.min.js"></script>
<script type="text/javascript" defer src="storefront/view/default/javascript/custom.js"></script>



	<script type="text/javascript" src="extensions/banner_manager/storefront/view/default/javascript/banner_manager.js"></script>
</div>

  <div id="bitnami-banner" data-banner-id="7e250">  <style>#bitnami-banner{z-index:100000;height:80px;padding:0px;width:120px;background:transparent;position:fixed;right:0px;bottom:0px;border:0px solid #ededed}#bitnami-banner .bitnami-corner-image-div{position:fixed;right:0px;bottom:0px;border:0px;z-index:100001;height:110px}#bitnami-banner .bitnami-corner-image-div .bitnami-corner-image{position:fixed;right:0px;bottom:0px;border:0px;z-index:100001;height:110px}#bitnami-close-banner-button{height:12px;width:12px;z-index:10000000000;position:fixed;right:5px;bottom:65px;display:none;cursor:pointer}</style>  <img id="bitnami-close-banner-button" src="/bitnami/images/close.png" pagespeed_url_hash="1697578455" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/>  <div class="bitnami-corner-image-div">     <a href="/bitnami/index.html" target="_blank">       <img class="bitnami-corner-image" alt="Bitnami" src="/bitnami/images/corner-logo.png" pagespeed_url_hash="164046942" onload="pagespeed.CriticalImages.checkImageForCriticality(this);"/>     </a>  </div>  <script type="text/javascript" src="/bitnami/banner.js"></script> </div>   </body></html>